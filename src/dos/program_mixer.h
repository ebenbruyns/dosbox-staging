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
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef DOSBOX_PROGRAM_MIXER_H
#define DOSBOX_PROGRAM_MIXER_H

#include "programs.h"

#include <memory>
#include <optional>
#include <queue>
#include <string>
#include <variant>
#include <vector>

#include "audio_frame.h"
#include "mixer.h"

constexpr auto MasterMixerChannelName = "MASTER";

struct SelectChannel {
	std::string channel_name = {};
};

struct SetVolume {
	AudioFrame volume = {};
};

struct SetStereoMode {
	StereoLine mode = {};
};

struct SetCrossfeedStrength {
	float strength = {};
};

struct SetReverbLevel {
	float level = {};
};

struct SetChorusLevel {
	float level = {};
};

using MixerCommand = std::variant<SelectChannel, SetVolume, SetStereoMode,
                                  SetCrossfeedStrength, SetReverbLevel, SetChorusLevel>;

struct MixerCommandExecutor {
	void operator()(const SelectChannel& cmd);
	void operator()(const SetVolume& cmd);
	void operator()(const SetStereoMode& cmd);
	void operator()(const SetCrossfeedStrength& cmd);
	void operator()(const SetReverbLevel& cmd);
	void operator()(const SetChorusLevel& cmd);

private:
	// 'master_channel' is true if the MASTER channel is selected,
	// otherwise 'channel' points to the current non-master channel.
	bool master_channel = false;

	std::shared_ptr<MixerChannel> channel = {};
};

class MIXER final : public Program {
public:
	MIXER()
	{
		AddMessages();
		help_detail = {HELP_Filter::Common,
		               HELP_Category::Dosbox,
		               HELP_CmdType::Program,
		               "MIXER"};
	}
	void Run() override;

private:
	void ShowMixerStatus();

	static void AddMessages();

	std::queue<MixerCommand> ParseCommands(const std::vector<std::string>& args);

	void ExecuteCommands(MixerCommandExecutor& executor,
	                     std::queue<MixerCommand> commands);
};

#endif
