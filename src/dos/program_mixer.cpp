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

bool SelectChannel::operator==(const SelectChannel& that) const
{
	return channel_name == that.channel_name;
}

bool SetVolume::operator==(const SetVolume& that) const
{
	return volume == that.volume;
}

bool SetStereoMode::operator==(const SetStereoMode& that) const
{
	return lineout_map == that.lineout_map;
}

bool SetCrossfeedStrength::operator==(const SetCrossfeedStrength& that) const
{
	return strength == that.strength;
}

bool SetReverbLevel::operator==(const SetReverbLevel& that) const
{
	return level == that.level;
}

bool SetChorusLevel::operator==(const SetChorusLevel& that) const
{
	return level == that.level;
}

void Executor::operator()(const SelectChannel& cmd)
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

void Executor::operator()(const SetVolume& cmd)
{
	if (master_channel) {
		MIXER_SetMasterVolume(cmd.volume);
	} else {
		assert(channel);
		channel->SetUserVolume({cmd.volume.left, cmd.volume.right});
	}
}

void Executor::operator()(const SetStereoMode& cmd)
{
	assert(channel);
	channel->SetLineoutMap(cmd.lineout_map);
}

void Executor::operator()(const SetCrossfeedStrength& cmd)
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

void Executor::operator()(const SetReverbLevel& cmd)
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

void Executor::operator()(const SetChorusLevel& cmd)
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

std::optional<float> parse_percentage(const std::string& s)
{
	constexpr auto min_percentage = 0.0f;
	constexpr auto max_percentage = 100.0f;
	return parse_float(s);
}

std::optional<float> parse_prefixed_value(const char prefix, const std::string& s,
                                          const float min_value, const float max_value)
{
	if (s.size() <= 1 || !ciequals(s[0], prefix)) {
		return {};
	}

	return parse_float(s.substr(1));
}

// Parse the volume in string form, either in stereo or mono format,
// and possibly in decibel format, which is prefixed with a 'd'.
static std::optional<AudioFrame> parse_volume(const std::string& s)
{
	auto to_volume = [](const std::string& s) -> std::optional<float> {
		// Try parsing the volume from a percent value
		constexpr auto min_percent = 0.0f;
		constexpr auto max_percent = 9999.0f;

		if (const auto p = parse_float(s); p) {
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
			LOG_TRACE("parse_volume: %g", *v);
			return AudioFrame(*v, *v);
		}
	} else if (parts.size() == 2) {
		// Stereo volume value
		const auto l = to_volume(parts[0]);
		const auto r = to_volume(parts[1]);
		if (l && r) {
			LOG_TRACE("parse_volume: left: %g, right: %g", *l, *r);
			return AudioFrame(*l, *r);
		}
	}

	return {};
}

constexpr auto CrossfeedCommandPrefix = 'X';
constexpr auto ReverbCommandPrefix    = 'R';
constexpr auto ChorusCommandPrefix    = 'C';

static bool is_xfeed_reverb_or_chorus_command(const std::string& arg)
{
	if (arg.size() >= 2) {
		const auto command_prefix = arg[0];
		return (std::isdigit(arg[1]) &&
		        (command_prefix == CrossfeedCommandPrefix ||
		         command_prefix == ReverbCommandPrefix ||
		         command_prefix == ChorusCommandPrefix));
	}
	return false;
}

static std::variant<Error, Command> parse_xfeed_reverb_chorus_command(const std::string& arg)
{
	assert(arg.size() >= 1);

	auto parse_command_arg = [&]() -> std::optional<float> {
		if (const auto p = parse_percentage(arg.substr(1)); p) {
			return percentage_to_gain(*p);
		}
		return {};
	};

	const auto command_prefix = arg[0];

	switch (command_prefix) {
	case CrossfeedCommandPrefix:
		if (const auto strength = parse_command_arg(); strength) {
			const SetCrossfeedStrength cmd = {*strength};
			return cmd;
		} else {
			const Error error = {ErrorType::InvalidSetCrossfeedStrengthCommand,
			                     format_string("Invalid set crossfeed commmand: %s",
			                                   arg.c_str())};
			return error;
		}

	case ReverbCommandPrefix:
		if (const auto level = parse_command_arg(); level) {
			const SetReverbLevel cmd = {*level};
			return cmd;
		} else {
			const Error error = {ErrorType::InvalidSetReverbLevelCommand,
			                     format_string("Invalid set reverb level commmand: %s",
			                                   arg.c_str())};
			return error;
		}

	case ChorusCommandPrefix:
		if (const auto level = parse_command_arg(); level) {
			const SetChorusLevel cmd = {*level};
			return cmd;
		} else {
			const Error error = {ErrorType::InvalidSetChorusLevelCommand,
			                     format_string("Invalid set chorus level commmand: %s",
			                                   arg.c_str())};
			return error;
		}

	default:
		assert(false);
		const Error error = {ErrorType::ChannelNotFound, "dummy"};
		return error;
	}
}

static std::optional<StereoLine> parse_stereo_mode(const std::string& arg)
{
	if (arg == "STEREO") {
		return Stereo;
	}
	if (arg == "REVERSE") {
		return Reverse;
	}
	return {};
}

std::variant<Error, std::queue<Command>> ParseCommands(
        const std::vector<std::string>& args,
        const std::vector<std::string>& channel_names)
{
	// Argument parse states
	enum { Global, Master, Channel };

	auto state                      = Global;
	std::string curr_channel_name   = {};
	auto curr_channel_command_count = 0;

	std::queue<Command> commands = {};

	// We always implicitly select the "global virtual channel" at the start
	const SelectChannel cmd = {GlobalVirtualChannelName};
	commands.emplace(cmd);

	auto parse_select_channel_command =
	        [&](const std::string& arg) -> std::optional<SelectChannel> {
		const auto channel_name = arg;

		if (channel_name == MasterChannelName) {
			state             = Master;
			curr_channel_name = channel_name;

			const SelectChannel cmd = {channel_name};
			return cmd;
		}
		if (std::find(channel_names.begin(), channel_names.end(), channel_name) !=
		    channel_names.end()) {
			state             = Channel;
			curr_channel_name = channel_name;

			const SelectChannel cmd = {channel_name};
			return cmd;
		}
		return {};
	};

	auto error = [&](const ErrorType type, const std::string& message) {
		const Error error = {type, message};
		return error;
	};

	for (const auto& argument : args) {
		auto arg = argument;
		upcase(arg);

		LOG_TRACE("arg: %s", arg.c_str());

		switch (state) {
		case Global: {
			LOG_TRACE("state = Global");

			if (const auto volume = parse_volume(arg); volume) {
				return Error{ErrorType::InvalidGlobalCommand,
				             "A channel must be selected before setting the volume"};

			} else if (const auto mode = parse_stereo_mode(arg); mode) {
				return Error{ErrorType::InvalidGlobalCommand,
				             "A channel must be selected before setting the lineout mode"};

			} else if (is_xfeed_reverb_or_chorus_command(arg)) {
				const auto result = parse_xfeed_reverb_chorus_command(
				        arg);

				if (auto cmd = std::get_if<Command>(&result); cmd) {
					commands.emplace(*cmd);
					++curr_channel_command_count;
				} else {
					return std::get<Error>(result);
				}

			} else if (const auto cmd = parse_select_channel_command(arg);
			           cmd) {
				commands.emplace(*cmd);
				curr_channel_command_count = 0;

			} else {
				const auto message = format_string("Channel %s not found",
				                                   arg.c_str());
				return Error{ErrorType::ChannelNotFound, message};
			}
		} break;

		case Master: {
			LOG_TRACE("state = Master");

			if (const auto volume = parse_volume(arg); volume) {
				const SetVolume cmd = {*volume};
				commands.emplace(cmd);
				++curr_channel_command_count;

			} else if (const auto mode = parse_stereo_mode(arg); mode) {
				return Error{ErrorType::InvalidMasterCommand,
				             "Cannot set lineout mode of the MASTER channel"};

			} else if (is_xfeed_reverb_or_chorus_command(arg)) {
				return Error{ErrorType::InvalidMasterCommand,
				             "Cannot set reverb, chorus, or crossfeed "
				             "for the MASTER channel"};

			} else if (const auto channel = parse_select_channel_command(arg);
			           channel) {
				commands.emplace(*channel);
				curr_channel_command_count = 0;

			} else {
				const auto message = format_string("Channel %s not found",
				                                   arg.c_str());
				return Error{ErrorType::ChannelNotFound, message};
			}
		} break;

		case Channel: {
			LOG_TRACE("state = Channel");

			if (const auto volume = parse_volume(arg); volume) {
				const SetVolume cmd = {*volume};
				commands.emplace(cmd);
				++curr_channel_command_count;

			} else if (const auto mode = parse_stereo_mode(arg); mode) {
				const SetStereoMode cmd = {*mode};
				commands.emplace(cmd);
				++curr_channel_command_count;

			} else if (is_xfeed_reverb_or_chorus_command(arg)) {
				const auto result = parse_xfeed_reverb_chorus_command(
				        arg);

				if (auto cmd = std::get_if<Command>(&result); cmd) {
					commands.emplace(*cmd);
					++curr_channel_command_count;
				} else {
					return std::get<Error>(result);
				}

			} else if (curr_channel_command_count > 0) {
				if (const auto channel = parse_select_channel_command(arg);
				    channel) {
					commands.emplace(*channel);
					curr_channel_command_count = 0;
				} else {
					const auto message = format_string(
					        "Channel %s not found", arg.c_str());
					return error(ErrorType::ChannelNotFound,
					             message);
				}
			} else {
				const auto message = format_string(
				        "Missing command after %s channel",
				        curr_channel_name.c_str());
				return error(ErrorType::MissingChannelCommand,
				             message);
			}
		} break;
		}
	}

	if (curr_channel_command_count == 0) {
		const auto message = format_string("Missing command after the '%s' channel",
		                                   curr_channel_name.c_str());
		return error(ErrorType::MissingChannelCommand, message);
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

static std::vector<std::string> get_channel_names()
{
	std::vector<std::string> names = {};

	for (const auto& [name, _] : MIXER_GetChannels()) {
		names.emplace_back(name);
	}
	return names;
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

	auto result = MixerCommand::ParseCommands(args, get_channel_names());

	if (auto commands = std::get_if<std::queue<MixerCommand::Command>>(&result);
	    commands) {
		MixerCommand::Executor executor = {};
		MixerCommand::ExecuteCommands(executor, *commands);

		if (show_status) {
			ShowMixerStatus();
			WriteOut("\n");
		}
	} else {
		auto error = std::get<MixerCommand::Error>(result);
		if (show_status) {
			ShowMixerStatus();
			WriteOut("\n");
		}
		const auto error_message = error.message.c_str();
		WriteOut("%s\n", error_message);
		LOG_WARNING("MIXER: %s", error_message);
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
