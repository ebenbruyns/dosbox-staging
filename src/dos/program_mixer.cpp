/*
 *  SPDX-License-Identifier: GPL-2.0-or-later
 *
 *  Copyright (C) 2023-2023  The DOSBox Staging Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include "program_mixer.h"

#include "ansi_code_markup.h"
#include "audio_frame.h"
#include "checks.h"
#include "math_utils.h"
#include "midi.h"
#include "string_utils.h"

CHECK_NARROWING();

void MixerCommandExecutor::operator()(const SelectChannel& cmd)
{
	if (cmd.channel_name == MasterMixerChannelName) {
		master_channel = true;
		channel        = nullptr;
	} else {
		master_channel = false;
		channel        = MIXER_FindChannel(cmd.channel_name.c_str());
		assert(channel);
	}
}

void MixerCommandExecutor::operator()(const SetVolume& cmd)
{
	if (master_channel) {
		MIXER_SetMasterVolume(cmd.volume);
	} else {
		channel->SetUserVolume(cmd.volume.left, cmd.volume.right);
	}
}

void MixerCommandExecutor::operator()(const SetStereoMode& cmd)
{
	if (master_channel) {
		// TODO
	} else {
		// TODO
	}
}

void MixerCommandExecutor::operator()(const SetCrossfeedStrength& cmd)
{
	if (master_channel) {
		for (const auto& [_, channel] : MIXER_GetChannels()) {
			channel->SetCrossfeedStrength(cmd.strength);
		}
	} else {
		channel->SetCrossfeedStrength(cmd.strength);
	}
}

void MixerCommandExecutor::operator()(const SetReverbLevel& cmd)
{
	// Enable reverb if it's disabled
	if (MIXER_GetReverbPreset() == ReverbPreset::None) {
		MIXER_SetReverbPreset(DefaultReverbPreset);
	}

	if (master_channel) {
		for (const auto& [_, channel] : MIXER_GetChannels()) {
			channel->SetReverbLevel(cmd.level);
		}
	} else {
		channel->SetReverbLevel(cmd.level);
	}
}

void MixerCommandExecutor::operator()(const SetChorusLevel& cmd)
{
	// Enable chorus if it's disabled
	if (MIXER_GetChorusPreset() == ChorusPreset::None) {
		MIXER_SetChorusPreset(DefaultChorusPreset);
	}

	if (master_channel) {
		for (const auto& [_, channel] : MIXER_GetChannels()) {
			channel->SetChorusLevel(cmd.level);
		}
	} else {
		channel->SetChorusLevel(cmd.level);
	}
}

// Parse the volume in string form, either in stereo or mono format,
// and possibly in decibel format, which is prefixed with a 'd'.
static std::optional<AudioFrame> parse_volume(const std::string& s)
{
	auto to_volume = [](const std::string& s) -> std::optional<float> {
		// Try parsing the volume from a percent value
		constexpr auto min_percent = 0.0f;
		constexpr auto max_percent = 9999.0f;

		if (const auto p = parse_value(s, min_percent, max_percent); p) {
			return percentage_to_gain(*p);
		}

		// Try parsing the volume from a decibel value
		constexpr auto min_db         = -40.00f;
		constexpr auto max_db         = 39.999f;
		constexpr auto decibel_prefix = 'd';

		if (const auto d = parse_prefixed_value(decibel_prefix, s, min_db, max_db);
		    d) {
			return decibel_to_gain(*d);
		}

		return {};
	};

	auto parts = split(s, ':');

	if (parts.size() == 1) {
		// Single volume value
		if (const auto v = to_volume(parts[0]); v) {
			return AudioFrame(*v, *v);
		}
	} else if (parts.size() == 2) {
		// Stereo volume value
		const auto l = to_volume(parts[0]);
		const auto r = to_volume(parts[1]);
		if (l && r) {
			return AudioFrame(*l, *r);
		}
	}

	return {};
}

std::queue<MixerCommand> MIXER::ParseCommands(const std::vector<std::string>& args)
{
	mixer_channel_t channel    = {};
	bool is_master             = false;
	auto channel_command_count = 0;

	// Argument parse states
	enum { Global, Master, SelectChannel, Channel };
	auto state = Global;

	constexpr auto crossfeed_command = 'X';
	constexpr auto reverb_command    = 'R';
	constexpr auto chorus_command    = 'C';

	std::queue<MixerCommand> commands = {};

	auto parse_channel_name = [&](const std::string& arg) -> bool {
		bool found = false;
		if (arg == MasterMixerChannelName) {
			found     = true;
			channel   = nullptr;
			is_master = true;
		} else {
			if (auto chan = MIXER_FindChannel(arg.c_str()); chan) {
				found     = true;
				channel   = chan;
				is_master = false;
			}
		}
		if (found) {
			state = is_master ? Master : Channel;

			channel_command_count = 0;
			return true;
		}
		return false;
	};

	for (const auto& argument : args) {
		auto arg = argument;
		upcase(arg);

		switch (state) {
		case Global: {
			if (parse_channel_name(arg)) {
				state = is_master ? Master : Channel;
				continue;
			}

			// Global commands apply to all non-master channels
			if (auto p = parse_prefixed_percentage(crossfeed_command, arg);
			    p) {
				for (auto& it : MIXER_GetChannels()) {
					const auto strength = percentage_to_gain(*p);
					it.second->SetCrossfeedStrength(strength);
				}

			} else if (p = parse_prefixed_percentage(reverb_command, arg);
			           p) {
				const auto level = percentage_to_gain(*p);
				//				set_reverb_level(level,
				// set_of_channels());

			} else if (p = parse_prefixed_percentage(chorus_command, arg);
			           p) {
				const auto level = percentage_to_gain(*p);
				//				set_chorus_level(level,
				// set_of_channels());

			} else {
				// WriteOut("%s%s",
				// MSG_Get("SHELL_CMD_MIXER_CHANNEL_NOT_FOUND"),
				// arg.c_str());
				WriteOut("Invalid channel name or global command: %s",
				         arg.c_str());
				break;
			}
		} break;

		case Master: {
			// Only setting the volume is allowed for the
			// master channel
			if (const auto v = parse_volume(arg); v) {
				MIXER_SetMasterVolume(*v);
				// Only a select channel command is allowed
				// after this
				state = SelectChannel;
			} else {
				WriteOut("Invalid master command: %s", arg.c_str());
				break;
			}
		} break;

		case SelectChannel: {
			if (parse_channel_name(arg)) {
				state = is_master ? Master : Channel;
				continue;
			} else {
				WriteOut("Invalid channel name: %s", arg.c_str());
				break;
			}
		}

		case Channel: {
			// Adjust settings of a regular non-master channel
			if (auto p = parse_prefixed_percentage(crossfeed_command, arg);
			    p) {
				const auto strength = percentage_to_gain(*p);
				channel->SetCrossfeedStrength(strength);

			} else if (p = parse_prefixed_percentage(reverb_command, arg);
			           p) {
				const auto level = percentage_to_gain(*p);
				//				set_reverb_level(level,
				//{channel});

			} else if (p = parse_prefixed_percentage(chorus_command, arg);
			           p) {
				const auto level = percentage_to_gain(*p);
				//				set_chorus_level(level,
				//{channel});

			} else if (channel->ChangeLineoutMap(arg)) {
				// nop
			} else if (const auto v = parse_volume(arg); v) {
				channel->SetUserVolume(v->left, v->right);
			}

			if (channel_command_count > 0) {
				if (parse_channel_name(arg)) {
					state = is_master ? Master : Channel;
					continue;
				}
			}

			WriteOut("Invalid channel command: %s", arg.c_str());
			break;
		} break;
		}
	}

	return commands;
}

void MIXER::ExecuteCommands(MixerCommandExecutor& executor,
                            std::queue<MixerCommand> commands)
{
	while (!commands.empty()) {
		std::visit(executor, commands.front());
		commands.pop();
	}
}

void MIXER::Run()
{
	if (HelpRequested()) {
		WriteOut(MSG_Get("SHELL_CMD_MIXER_HELP_LONG"));
		return;
	}

	if (cmd->FindExist("/LISTMIDI")) {
		MIDI_ListAll(this);
		return;
	}

	constexpr auto remove = false;
	auto showStatus       = !cmd->FindExist("/NOSHOW", remove);

	const auto args = cmd->GetArguments();

	MIXER_LockAudioDevice();

	MixerCommandExecutor executor = {};

	auto commands = ParseCommands(args);
	ExecuteCommands(executor, commands);

	MIXER_UnlockAudioDevice();

	MIXER_UpdateAllChannelVolumes();

	if (showStatus) {
		ShowMixerStatus();
	}
}

void MIXER::AddMessages()
{
	MSG_Add("SHELL_CMD_MIXER_HELP_LONG",
	        "Displays or changes the sound mixer settings.\n"
	        "\n"
	        "Usage:\n"
	        "  [color=green]mixer[reset] [color=cyan][CHANNEL][reset] [color=white]COMMANDS[reset] [/noshow]\n"
	        "  [color=green]mixer[reset] [/listmidi]\n"
	        "\n"
	        "Where:\n"
	        "  [color=cyan]CHANNEL[reset]  is the sound channel to change the settings of.\n"
	        "  [color=white]COMMANDS[reset] is one or more of the following commands:\n"
	        "    Volume:    [color=white]0[reset] to [color=white]100[reset], or decibel value prefixed with [color=white]d[reset] (e.g. [color=white]d-7.5[reset])\n"
	        "               use [color=white]L:R[reset] to set the left and right side separately (e.g. [color=white]10:20[reset])\n"
	        "    Lineout:   [color=white]stereo[reset], [color=white]reverse[reset] (for stereo channels only)\n"
	        "    Crossfeed: [color=white]x0[reset] to [color=white]x100[reset]    Reverb: [color=white]r0[reset] to [color=white]r100[reset]    Chorus: [color=white]c0[reset] to [color=white]c100[reset]\n"
	        "Notes:\n"
	        "  - Run [color=green]mixer[reset] without arguments to view the current settings.\n"
	        "  - You may change the settings of more than one channel in a single command.\n"
	        "  - If channel is unspecified, you can set crossfeed, reverb, or chorus\n"
	        "    globally for all channels.\n"
	        "  - Run [color=green]mixer[reset] /listmidi to list all available MIDI devices.\n"
	        "  - The /noshow option applies the changes without showing the mixer settings.\n"
	        "\n"
	        "Examples:\n"
	        "  [color=green]mixer[reset] [color=cyan]cdaudio[reset] [color=white]50[reset] [color=cyan]sb[reset] [color=white]reverse[reset] /noshow\n"
	        "  [color=green]mixer[reset] [color=white]x30[reset] [color=cyan]opl[reset] [color=white]150 r50 c30[reset] [color=cyan]sb[reset] [color=white]x10[reset]");

	MSG_Add("SHELL_CMD_MIXER_HEADER_LAYOUT",
	        "%-22s %4.0f:%-4.0f %+6.2f:%-+6.2f  %-8s %5s %7s %7s");

	MSG_Add("SHELL_CMD_MIXER_HEADER_LABELS",
	        "[color=white]Channel      Volume    Volume (dB)   Mode     Xfeed  Reverb  Chorus[reset]");

	MSG_Add("SHELL_CMD_MIXER_CHANNEL_OFF", "off");
	MSG_Add("SHELL_CMD_MIXER_CHANNEL_STEREO", "Stereo");
	MSG_Add("SHELL_CMD_MIXER_CHANNEL_REVERSE", "Reverse");
	MSG_Add("SHELL_CMD_MIXER_CHANNEL_MONO", "Mono");
	MSG_Add("SHELL_CMD_MIXER_CHANNEL_NOT_FOUND", "Channel not found: ");
}

void MIXER::ShowMixerStatus()
{
	std::string column_layout = MSG_Get("SHELL_CMD_MIXER_HEADER_LAYOUT");
	column_layout.append({'\n'});

	auto show_channel = [&](const std::string& name,
	                        const AudioFrame& volume,
	                        const std::string& mode,
	                        const std::string& xfeed,
	                        const std::string& reverb,
	                        const std::string& chorus) {
		WriteOut(column_layout.c_str(),
		         name.c_str(),
		         static_cast<double>(volume.left * 100.0f),
		         static_cast<double>(volume.right * 100.0f),
		         static_cast<double>(gain_to_decibel(volume.left)),
		         static_cast<double>(gain_to_decibel(volume.right)),
		         mode.c_str(),
		         xfeed.c_str(),
		         reverb.c_str(),
		         chorus.c_str());
	};

	WriteOut("%s\n", MSG_Get("SHELL_CMD_MIXER_HEADER_LABELS"));

	const auto off_value      = MSG_Get("SHELL_CMD_MIXER_CHANNEL_OFF");
	constexpr auto none_value = "-";

	MIXER_LockAudioDevice();

	constexpr auto master_channel_string = "[color=cyan]MASTER[reset]";

	show_channel(convert_ansi_markup(master_channel_string),
	             MIXER_GetMasterVolume(),
	             MSG_Get("SHELL_CMD_MIXER_CHANNEL_STEREO"),
	             none_value,
	             none_value,
	             none_value);

	for (auto& [name, chan] : MIXER_GetChannels()) {
		std::string xfeed = none_value;
		if (chan->HasFeature(ChannelFeature::Stereo)) {
			if (chan->GetCrossfeedStrength() > 0.0f) {
				xfeed = std::to_string(static_cast<uint8_t>(round(
				        chan->GetCrossfeedStrength() * 100.0f)));
			} else {
				xfeed = off_value;
			}
		}

		std::string reverb = none_value;
		if (chan->HasFeature(ChannelFeature::ReverbSend)) {
			if (chan->GetReverbLevel() > 0.0f) {
				reverb = std::to_string(static_cast<uint8_t>(
				        round(chan->GetReverbLevel() * 100.0f)));
			} else {
				reverb = off_value;
			}
		}

		std::string chorus = none_value;
		if (chan->HasFeature(ChannelFeature::ChorusSend)) {
			if (chan->GetChorusLevel() > 0.0f) {
				chorus = std::to_string(static_cast<uint8_t>(
				        round(chan->GetChorusLevel() * 100.0f)));
			} else {
				chorus = off_value;
			}
		}

		auto channel_name = std::string("[color=cyan]") + name +
		                    std::string("[reset]");

		auto mode = chan->DescribeLineout();

		show_channel(convert_ansi_markup(channel_name),
		             chan->GetUserVolume(),
		             mode,
		             xfeed,
		             reverb,
		             chorus);
	}

	MIXER_UnlockAudioDevice();
}
