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

#include <cctype>

#include "ansi_code_markup.h"
#include "audio_frame.h"
#include "checks.h"
#include "math_utils.h"
#include "midi.h"
#include "string_utils.h"

CHECK_NARROWING();

namespace MixerCommand {

bool SelectChannel::operator==(const SelectChannel that) const
{
	return channel_name == that.channel_name;
}

bool SetVolume::operator==(const SetVolume that) const
{
	return volume == that.volume;
}

bool SetStereoMode::operator==(const SetStereoMode that) const
{
	return lineout_map == that.lineout_map;
}

bool SetCrossfeedStrength::operator==(const SetCrossfeedStrength that) const
{
	return strength == that.strength;
}

bool SetReverbLevel::operator==(const SetReverbLevel that) const
{
	return level == that.level;
}

bool SetChorusLevel::operator==(const SetChorusLevel that) const
{
	return level == that.level;
}

void Executor::operator()(const SelectChannel cmd)
{
	global_command = false;
	master_channel = false;
	channel        = nullptr;

	if (cmd.channel_name == GlobalVirtualChannelName) {
		global_command = true;
	} else if (cmd.channel_name == MasterChannelName) {
		master_channel = true;
	} else {
		channel = MIXER_FindChannel(cmd.channel_name.c_str());
		assert(channel);
	}
}

void Executor::operator()(const SetVolume cmd)
{
	if (master_channel) {
		MIXER_SetMasterVolume(cmd.volume);
	} else {
		assert(channel);
		channel->SetUserVolume({cmd.volume.left, cmd.volume.right});
	}
}

void Executor::operator()(const SetStereoMode cmd)
{
	assert(channel);
	channel->SetLineoutMap(cmd.lineout_map);
}

void Executor::operator()(const SetCrossfeedStrength cmd)
{
	if (global_command) {
		for (const auto& [_, channel] : MIXER_GetChannels()) {
			channel->SetCrossfeedStrength(cmd.strength);
		}
	} else {
		assert(channel);
		channel->SetCrossfeedStrength(cmd.strength);
	}
}

void Executor::operator()(const SetReverbLevel cmd)
{
	// Enable reverb if it's disabled
	if (MIXER_GetReverbPreset() == ReverbPreset::None) {
		MIXER_SetReverbPreset(DefaultReverbPreset);
	}

	if (global_command) {
		for (const auto& [_, channel] : MIXER_GetChannels()) {
			channel->SetReverbLevel(cmd.level);
		}
	} else {
		assert(channel);
		channel->SetReverbLevel(cmd.level);
	}
}

void Executor::operator()(const SetChorusLevel cmd)
{
	// Enable chorus if it's disabled
	if (MIXER_GetChorusPreset() == ChorusPreset::None) {
		MIXER_SetChorusPreset(DefaultChorusPreset);
	}

	if (global_command) {
		for (const auto& [_, channel] : MIXER_GetChannels()) {
			channel->SetChorusLevel(cmd.level);
		}
	} else {
		assert(channel);
		channel->SetChorusLevel(cmd.level);
	}
}

std::optional<float> parse_percentage(const std::string& s,
                                      const float min_percent = 0.0f,
                                      const float max_percent = 100.0f)
{
	if (const auto p = parse_float(s); p) {
		if (*p >= min_percent && *p <= max_percent) {
			return percentage_to_gain(*p);
		}
	}
	return {};
}

static bool is_start_of_number(const char c)
{
	return (c == '-' || c == '+' || std::isdigit(c));
}

constexpr auto DecibelVolumeCommandPrefix = 'D';

static bool is_volume_command(const std::string& s)
{
	if (s.size() < 1) {
		return false;
	}
	auto is_percent_volume_command = [&]() {
		return is_start_of_number(s[0]);
	};
	auto is_decibel_volume_command = [&]() {
		return (s[0] == DecibelVolumeCommandPrefix);
	};
	return is_percent_volume_command() || is_decibel_volume_command();
}

static std::variant<Error, Command> parse_volume_command(const std::string& s)
{
	constexpr auto MinDb = -96.00f;
	constexpr auto MaxDb = 40.000f;

	static const auto MinPercent = decibel_to_gain(MinDb);

	// Almost 40 dB, just a *tiny* bit below to ensure that the mixer
	// columns don't get too wide.
	static const auto MaxPercent = percentage_to_gain(9999.0f);

	auto parse_percent_volume = [&](const std::string& s) -> std::optional<float> {
		// Allow setting the volume to absolute silence (-inf dB) when
		// specifying percentage volumes
		if (const auto p = parse_percentage(s, 0.0f, MaxPercent); p) {
			return *p;
		}
		return {};
	};

	auto parse_decibel_volume = [&](const std::string& s) -> std::optional<float> {
		if (s[0] != DecibelVolumeCommandPrefix) {
			return {};
		};
		if (const auto d = parse_float(s.substr(1));
		    d && (*d >= MinDb && *d <= MaxDb)) {
			return decibel_to_gain(*d);
		}
		return {};
	};

	auto parse_volume = [&](const std::string& s) -> std::optional<float> {
		if (s.size() < 1) {
			return {};
		}
		auto v = parse_percent_volume(s);
		if (!v) {
			v = parse_decibel_volume(s);
		}
		if (!v) {
			return {};
		}

		// Allow setting the volume to absolute silence (-inf dB) if a
		// percentage volume of '0' was specified...
		if (*v == 0.0f) {
			return *v;
		}
		// ...but clamp to the [-96dB, 40dB] range otherwise (40dB would
		// be a 10000 percentage value, but we clamp to 9999 instead
		// because the tabular mixer output looks better that way).
		return std::clamp(*v, MinPercent, MaxPercent);
	};

	auto parts = split(s, ':');

	if (parts.size() == 1) {
		// Single volume value for both channels (e.g. 10)
		if (const auto v = parse_volume(parts[0]); v) {
			const SetVolume cmd = {AudioFrame(*v, *v)};
			return cmd;
		} else {
			const Error error = {ErrorType::InvalidVolume,
			                     format_string(MSG_Get("SHELL_CMD_MIXER_INVALID_VOLUME_COMMAND"),
			                                   s.c_str())};
			return error;
		}

	} else if (parts.size() == 2) {
		// Colon-separated stereo volume value (e.g. 10:20)
		const auto l = parse_volume(parts[0]);
		const auto r = parse_volume(parts[1]);
		if (l && r) {
			const SetVolume cmd = {AudioFrame(*l, *r)};
			return cmd;
		} else {
			const Error error = {ErrorType::InvalidVolume,
			                     format_string(MSG_Get("SHELL_CMD_MIXER_INVALID_VOLUME_COMMAND"),
			                                   s.c_str())};
			return error;
		}
	} else { // more than 2 parts
		const Error error = {ErrorType::InvalidVolume,
		                     format_string(MSG_Get("SHELL_CMD_MIXER_INVALID_VOLUME_COMMAND"),
		                                   s.c_str())};
		return error;
	}
}

static std::optional<StereoLine> parse_stereo_mode(const std::string& s)
{
	if (s == "STEREO") {
		return Stereo;
	}
	if (s == "REVERSE") {
		return Reverse;
	}
	return {};
}

constexpr auto CrossfeedCommandPrefix = 'X';
constexpr auto ReverbCommandPrefix    = 'R';
constexpr auto ChorusCommandPrefix    = 'C';

static bool is_command_with_prefix(const std::string& s, const char prefix)
{
	if (s.size() < 2) {
		return false;
	}
	const auto command_prefix = s[0];
	return (command_prefix == prefix && is_start_of_number(s[1]));
}

static std::variant<Error, Command> parse_crossfeed_command(const std::string& s)
{
	assert(s.size() >= 2);

	if (const auto strength = parse_percentage(s.substr(1)); strength) {
		const SetCrossfeedStrength cmd = {*strength};
		return cmd;
	} else {
		const Error error = {ErrorType::InvalidCrossfeedStrength,
		                     format_string(MSG_Get("SHELL_CMD_MIXER_INVALID_CROSSFEED_STRENGTH"),
		                                   s.c_str())};
		return error;
	}
}

static std::variant<Error, Command> parse_reverb_command(const std::string& s)
{
	assert(s.size() >= 2);

	if (const auto level = parse_percentage(s.substr(1)); level) {
		const SetReverbLevel cmd = {*level};
		return cmd;
	} else {
		const Error error = {ErrorType::InvalidReverbLevel,
		                     format_string(MSG_Get("SHELL_CMD_MIXER_INVALID_REVERB_LEVEL"),
		                                   s.c_str())};
		return error;
	}
}

static std::variant<Error, Command> parse_chorus_command(const std::string& s)
{
	assert(s.size() >= 2);

	if (const auto level = parse_percentage(s.substr(1)); level) {
		const SetChorusLevel cmd = {*level};
		return cmd;
	} else {
		const Error error = {ErrorType::InvalidChorusLevel,
		                     format_string(MSG_Get("SHELL_CMD_MIXER_INVALID_REVERB_LEVEL"),
		                                   s.c_str())};
		return error;
	}
}

std::variant<Error, std::queue<Command>> ParseCommands(
        const std::vector<std::string>& args, const ChannelInfos& channel_infos)
{
	std::string curr_channel_name   = GlobalVirtualChannelName;
	auto curr_channel_command_count = 0;

	std::queue<Command> commands = {};

	// We always implicitly select the "global virtual channel" at the start
	const SelectChannel cmd = {GlobalVirtualChannelName};
	commands.emplace(cmd);

	auto parse_select_channel_command =
	        [&](const std::string& s) -> std::optional<SelectChannel> {
		const auto channel_name = s;

		if (channel_infos.find(channel_name) != channel_infos.end()) {
			curr_channel_name = channel_name;

			const SelectChannel cmd = {channel_name};
			return cmd;
		}
		return {};
	};

	auto has_feature = [&](const std::string& channel_name,
	                       const ChannelFeature feature) {
		if (auto it = channel_infos.find(channel_name);
		    it != channel_infos.end()) {
			const auto [_, features] = *it;
			return (features.find(feature) != features.end());
		}
		return false;
	};

	auto is_channel_mono = [&](const std::string& channel_name) {
		return !has_feature(channel_name, ChannelFeature::Stereo);
	};
	auto is_global_channel = [&](const std::string& channel_name) {
		return channel_name == GlobalVirtualChannelName;
	};
	auto is_master_channel = [&](const std::string& channel_name) {
		return channel_name == MasterChannelName;
	};

	auto error = [&](const ErrorType type, const std::string& message) {
		const Error error = {type, message};
		return error;
	};

	for (const auto& argument : args) {
		auto arg = argument;
		upcase(arg);

		LOG_TRACE("arg: %s", arg.c_str());
		LOG_TRACE("curr_channel_name: %s, cmd_count: %d",
		          curr_channel_name.c_str(),
		          curr_channel_command_count);

		if (is_volume_command(arg)) {
			if (is_global_channel(curr_channel_name)) {
				const auto message = MSG_Get(
				        "SHELL_CMD_MIXER_INVALID_GLOBAL_VOLUME_COMMAND");
				return error(ErrorType::InvalidGlobalCommand, message);
			}

			const auto result = parse_volume_command(arg);

			if (auto cmd = std::get_if<Command>(&result); cmd) {
				commands.emplace(*cmd);
				++curr_channel_command_count;
			} else {
				return std::get<Error>(result);
			}

		} else if (const auto mode = parse_stereo_mode(arg); mode) {
			if (is_global_channel(curr_channel_name)) {
				const auto message = MSG_Get(
				        "SHELL_CMD_MIXER_INVALID_GLOBAL_STEREO_MODE_COMMAND");
				return error(ErrorType::InvalidGlobalCommand, message);
			}
			if (is_master_channel(curr_channel_name)) {
				const auto message = format_string(
				        MSG_Get("SHELL_CMD_MIXER_INVALID_STEREO_MODE_COMMAND"),
				        curr_channel_name.c_str());

				return error(ErrorType::InvalidChannelCommand,
				             message);
			}

			const SetStereoMode cmd = {*mode};
			commands.emplace(cmd);
			++curr_channel_command_count;

		} else if (is_command_with_prefix(arg, CrossfeedCommandPrefix)) {
			if (is_channel_mono(curr_channel_name) ||
			    is_master_channel(curr_channel_name)) {
				const auto message = format_string(
				        MSG_Get("SHELL_CMD_MIXER_INVALID_CROSSFEED_COMMAND"),
				        curr_channel_name.c_str());

				return error(ErrorType::InvalidChannelCommand,
				             message);
			}

			const auto result = parse_crossfeed_command(arg);

			if (auto cmd = std::get_if<Command>(&result); cmd) {
				commands.emplace(*cmd);
				++curr_channel_command_count;
			} else {
				return std::get<Error>(result);
			}

		} else if (is_command_with_prefix(arg, ReverbCommandPrefix)) {
			if (is_channel_mono(curr_channel_name) ||
			    is_master_channel(curr_channel_name)) {
				const auto message = format_string(
				        MSG_Get("SHELL_CMD_MIXER_INVALID_REVERB_COMMAND"),
				        curr_channel_name.c_str());

				return error(ErrorType::InvalidChannelCommand,
				             message);
			}

			const auto result = parse_reverb_command(arg);

			if (auto cmd = std::get_if<Command>(&result); cmd) {
				commands.emplace(*cmd);
				++curr_channel_command_count;
			} else {
				return std::get<Error>(result);
			}

		} else if (is_command_with_prefix(arg, ChorusCommandPrefix)) {
			if (is_channel_mono(curr_channel_name) ||
			    is_master_channel(curr_channel_name)) {
				const auto message = format_string(
				        MSG_Get("SHELL_CMD_MIXER_INVALID_CHORUS_COMMAND"),
				        curr_channel_name.c_str());

				return error(ErrorType::InvalidChannelCommand,
				             message);
			}

			const auto result = parse_chorus_command(arg);

			if (auto cmd = std::get_if<Command>(&result); cmd) {
				commands.emplace(*cmd);
				++curr_channel_command_count;
			} else {
				return std::get<Error>(result);
			}

		} else if (is_global_channel(curr_channel_name) ||
		           curr_channel_command_count > 0) {
			if (const auto channel = parse_select_channel_command(arg);
			    channel) {
				commands.emplace(*channel);
				curr_channel_command_count = 0;
			} else {
				const auto message = format_string(
				        MSG_Get("SHELL_CMD_MIXER_CHANNEL_NOT_FOUND"),
				        arg.c_str());

				return error(ErrorType::ChannelNotFound, message);
			}
		} else {
			const auto message = format_string(
			        MSG_Get("SHELL_CMD_MIXER_MISSING_CHANNEL_COMMAND"),
			        curr_channel_name.c_str());

			return error(ErrorType::MissingCommand, message);
		}
	}

	if (curr_channel_command_count == 0) {
		const auto message = format_string(
		        MSG_Get("SHELL_CMD_MIXER_MISSING_CHANNEL_COMMAND"),
		        curr_channel_name.c_str());
		return error(ErrorType::MissingCommand, message);
	}

	return commands;
}

void ExecuteCommands(Executor& executor, std::queue<Command>& commands)
{
	while (!commands.empty()) {
		std::visit(executor, commands.front());
		commands.pop();
	}
}

} // namespace MixerCommand

static ChannelInfos get_channel_infos()
{
	ChannelInfos infos = {};

	infos[MasterChannelName] = {ChannelFeature::Stereo};

	for (const auto& [name, channel] : MIXER_GetChannels()) {
		infos[name] = channel->GetFeatures();
	}
	return infos;
}

void MIXER::Run()
{
	if (HelpRequested()) {
		WriteOut(MSG_Get("SHELL_CMD_MIXER_HELP_LONG"));
		return;
	}
	if (cmd->GetCount() == 0) {
		ShowMixerStatus();
		return;
	}
	if (cmd->FindExist("/LISTMIDI")) {
		MIDI_ListAll(this);
		return;
	}

	constexpr auto remove = false;
	auto show_status      = !cmd->FindExist("/NOSHOW", remove);

	const auto args = cmd->GetArguments();

	MIXER_LockAudioDevice();

	auto result = MixerCommand::ParseCommands(args, get_channel_infos());

	if (auto commands = std::get_if<std::queue<MixerCommand::Command>>(&result);
	    commands) {
		// Success (all mixer commands executed successfully)
		MixerCommand::Executor executor = {};
		MixerCommand::ExecuteCommands(executor, *commands);

		if (show_status) {
			ShowMixerStatus();
		}

	} else {
		// Error (no mixer command was executed)
		if (show_status) {
			ShowMixerStatus();
			WriteOut("\n");
		}
		auto error = std::get<MixerCommand::Error>(result);
		const auto error_message = error.message.c_str();
		WriteOut("%s\n", error_message);

		// To give people a hint if their [autoexec] contains invalid MIXER
		// commands.
		LOG_WARNING("MIXER: The MIXER command was invoked with incorrect arguments; "
		            "run MIXER /? for help");
	}

	MIXER_UnlockAudioDevice();
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
	        "    Volume:    [color=white]0[reset] to [color=white]9999[reset], or decibel value prefixed with [color=white]d[reset] (e.g. [color=white]d-7.5[reset])\n"
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

	MSG_Add("SHELL_CMD_MIXER_CHANNEL_NOT_FOUND",
	        "Channel [color=cyan]%s[reset] not found");

	MSG_Add("SHELL_CMD_MIXER_INVALID_GLOBAL_VOLUME_COMMAND",
	        "A channel must be selected before setting the volume");

	MSG_Add("SHELL_CMD_MIXER_INVALID_GLOBAL_STEREO_MODE_COMMAND",
	        "A channel must be selected before setting the stereo mode");

	MSG_Add("SHELL_CMD_MIXER_INVALID_STEREO_MODE_COMMAND",
	        "Cannot set the stereo mode of the [color=cyan]%s[reset] channel");

	MSG_Add("SHELL_CMD_MIXER_INVALID_CROSSFEED_COMMAND",
	        "Cannot set the crossfeed strength of the [color=cyan]%s[reset] channel");

	MSG_Add("SHELL_CMD_MIXER_INVALID_REVERB_COMMAND",
	        "Cannot set the reverb level of the the [color=cyan]%s[reset] channel");

	MSG_Add("SHELL_CMD_MIXER_INVALID_CHORUS_COMMAND",
	        "Cannot set the chorus level of the the [color=cyan]%s[reset] channel");

	MSG_Add("SHELL_CMD_MIXER_INVALID_VOLUME_COMMAND",
	        "Invalid set volume command: [color=white]%s[reset] "
	        "(run MIXER /? for help)");

	MSG_Add("SHELL_CMD_MIXER_INVALID_CROSSFEED_STRENGTH",
	        "Invalid crossfeed strength: [color=white]%s[reset] "
	        "(must be between 0 and 100)");

	MSG_Add("SHELL_CMD_MIXER_INVALID_CHORUS_LEVEL",
	        "Invalid chorus level: [color=white]%s[reset] "
	        "(must be between 0 and 100)");

	MSG_Add("SHELL_CMD_MIXER_INVALID_REVERB_LEVEL",
	        "Invalid reverb level: [color=white]%s[reset] "
	        "(must be between 0 and 100)");

	MSG_Add("SHELL_CMD_MIXER_MISSING_CHANNEL_COMMAND",
	        "Missing command after the [color=cyan]%s[reset] channel");
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
