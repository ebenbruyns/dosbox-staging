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

#include "../src/dos/program_mixer.h"

#include <gtest/gtest.h>

using namespace MixerCommand;

std::vector<std::string> ChannelNames = { "PCSPEAKER", "OPL", "SB", "GUS" };

static void assert_success(const std::vector<std::string>& args,
                           const std::queue<Command>& expected)
{
	const auto result = ParseCommands(args, ChannelNames);

	if (auto error = std::get_if<Error>(&result); error) {
		printf("*** TEST FAILED: ");
		printf(error->message.c_str());
		printf("\n");
		FAIL();
	} else {
		auto actual = std::get<std::queue<MixerCommand::Command>>(result);
		EXPECT_EQ(actual, expected);
	}
}

static void assert_failure(const std::vector<std::string>& args,
                           const ErrorType expected_error_type)
{
	const auto result = ParseCommands(args, ChannelNames);

	if (auto error = std::get_if<Error>(&result); error) {
		LOG_WARNING(error->message.c_str());
		EXPECT_EQ(error->type, expected_error_type);
	} else {
		printf("*** TEST FAILED: No error reported");
		FAIL();
	}
}

// Success cases
//
TEST(ProgramMixer, Global_SetReverbLevel)
{
	std::queue<Command> expected = {};
	expected.emplace(SelectChannel{GlobalVirtualChannelName});
	expected.emplace(SetReverbLevel{0.2f});

	assert_success({"r20"}, expected);
}

TEST(ProgramMixer, Global_SetChorusLevel)
{
	std::queue<Command> expected = {};
	expected.emplace(SelectChannel{GlobalVirtualChannelName});
	expected.emplace(SetChorusLevel{0.2f});

	assert_success({"c20"}, expected);
}

TEST(ProgramMixer, Global_SetCrossfeedStrength)
{
	std::queue<Command> expected = {};
	expected.emplace(SelectChannel{GlobalVirtualChannelName});
	expected.emplace(SetCrossfeedStrength{0.2f});

	assert_success({"x20"}, expected);
}

TEST(ProgramMixer, Global_SetAllValid)
{
	std::queue<Command> expected = {};
	expected.emplace(SelectChannel{GlobalVirtualChannelName});
	expected.emplace(SetReverbLevel{0.2f});
	expected.emplace(SetChorusLevel{0.1f});
	expected.emplace(SetCrossfeedStrength{0.3f});

	assert_success({"r20", "c10", "x30"}, expected);
}

TEST(ProgramMixer, Global_SetAllValidMultiple)
{
	std::queue<Command> expected = {};
	expected.emplace(SelectChannel{GlobalVirtualChannelName});
	expected.emplace(SetCrossfeedStrength{0.07f});
	expected.emplace(SetReverbLevel{0.08f});
	expected.emplace(SetCrossfeedStrength{0.30f});
	expected.emplace(SetChorusLevel{0.09f});
	expected.emplace(SetReverbLevel{0.20f});
	expected.emplace(SetCrossfeedStrength{0.10f});

	assert_success({"x7", "r8", "x30", "c9", "r20", "x10"}, expected);
}

TEST(ProgramMixer, Master_SetVolume)
{
	std::queue<Command> expected = {};
	expected.emplace(SelectChannel{GlobalVirtualChannelName});
	expected.emplace(SelectChannel{MasterChannelName});
	expected.emplace(SetVolume{AudioFrame(0.2f, 0.2f)});

	assert_success({"master", "20"}, expected);
}

TEST(ProgramMixer, Master_SetVolumeMultiple)
{
	std::queue<Command> expected = {};
	expected.emplace(SelectChannel{GlobalVirtualChannelName});
	expected.emplace(SelectChannel{MasterChannelName});
	expected.emplace(SetVolume{AudioFrame(0.1f, 0.1f)});
	expected.emplace(SetVolume{AudioFrame(0.2f, 0.2f)});

	assert_success({"master", "10", "20"}, expected);
}

// Failure cases
//
TEST(ProgramMixer, Master_NoCommand)
{
	assert_failure({"master"}, ErrorType::MissingChannelCommand);
}

TEST(ProgramMixer, Master_SetReverbLevelInvalid)
{
	assert_failure({"master", "r20"}, ErrorType::InvalidMasterCommand);
}

TEST(ProgramMixer, Master_SetChorusLevelInvalid)
{
	assert_failure({"master", "c20"}, ErrorType::InvalidMasterCommand);
}

TEST(ProgramMixer, Master_SetCrossfeedStrengthInvalid)
{
	assert_failure({"master", "x20"}, ErrorType::InvalidMasterCommand);
}

/*
TEST(ProgramMixer, SetMasterVolMinus1)
{
        std::queue<Command> expected = {};
        expected.emplace(SelectChannel{"MASTER"});
        expected.emplace(SetVolume{AudioFrame(0.2f, 0.2f)});

        assert_success({"master", "-1"}, expected);
}
*/
