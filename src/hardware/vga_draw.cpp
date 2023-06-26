/*
 *  Copyright (C) 2002-2021  The DOSBox Team
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

#include "dosbox.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <cstring>
#include <utility>

#include "../gui/render_scalers.h"
#include "../ints/int10.h"
#include "bitops.h"
#include "math_utils.h"
#include "mem_unaligned.h"
#include "pic.h"
#include "reelmagic/vga_passthrough.h"
#include "render.h"
#include "vga.h"
#include "video.h"

//#undef C_DEBUG
//#define C_DEBUG 1
//#define LOG(X,Y) LOG_MSG

typedef uint8_t * (* VGA_Line_Handler)(Bitu vidstart, Bitu line);

static VGA_Line_Handler VGA_DrawLine;

// Confirm the maximum dimensions accomodate VGA's sub-350 line scan doubling
constexpr auto max_scan_doubled_width  = 400;
constexpr auto max_scan_doubled_height = 350 - 1;
static_assert(SCALER_MAXHEIGHT >= SCALER_MAX_MUL_HEIGHT * max_scan_doubled_height);
static_assert(SCALER_MAXWIDTH >= SCALER_MAX_MUL_WIDTH * max_scan_doubled_width);

constexpr auto max_pixel_bytes = sizeof(uint32_t);
constexpr auto max_line_bytes  = SCALER_MAXWIDTH * max_pixel_bytes;

// The line buffer can be written in units up to RGB888 pixels (32-bit) size
alignas(uint32_t) static std::array<uint8_t, max_line_bytes> templine_buffer;
static auto TempLine = templine_buffer.data();

static uint8_t * VGA_Draw_1BPP_Line(Bitu vidstart, Bitu line) {
	const uint8_t *base = vga.tandy.draw_base + ((line & vga.tandy.line_mask) << vga.tandy.line_shift);

	uint16_t i = 0;
	for (Bitu x = vga.draw.blocks; x > 0; --x, ++vidstart) {
		Bitu val = base[(vidstart & (8 * 1024 -1))];
		write_unaligned_uint32_at(TempLine, i++, CGA_2_Table[val >> 4]);
		write_unaligned_uint32_at(TempLine, i++, CGA_2_Table[val & 0xf]);
	}
	return TempLine;
}

static uint8_t * VGA_Draw_2BPP_Line(Bitu vidstart, Bitu line) {
	const uint8_t *base = vga.tandy.draw_base + ((line & vga.tandy.line_mask) << vga.tandy.line_shift);

	uint16_t i = 0;
	for (Bitu x = 0; x < vga.draw.blocks; ++x) {
		Bitu val = base[vidstart & vga.tandy.addr_mask];
		++vidstart;
		write_unaligned_uint32_at(TempLine, i++, CGA_4_Table[val]);
	}
	return TempLine;
}

static uint8_t * VGA_Draw_2BPPHiRes_Line(Bitu vidstart, Bitu line) {
	const uint8_t *base = vga.tandy.draw_base + ((line & vga.tandy.line_mask) << vga.tandy.line_shift);

	uint16_t i = 0;
	for (Bitu x = 0; x < vga.draw.blocks; ++x) {
		Bitu val1 = base[vidstart & vga.tandy.addr_mask];
		++vidstart;
		Bitu val2 = base[vidstart & vga.tandy.addr_mask];
		++vidstart;
		write_unaligned_uint32_at(TempLine, i++,
		                          CGA_4_HiRes_Table[(val1 >> 4) | (val2 & 0xf0)]);
		write_unaligned_uint32_at( TempLine, i++,
		                          CGA_4_HiRes_Table[(val1 & 0x0f) | ((val2 & 0x0f) << 4)]);
	}
	return TempLine;
}

static uint8_t *VGA_Draw_CGA16_Line(Bitu vidstart, Bitu line)
{
	// TODO: De-Bitu the VGA API as line indexes are well-under 65k/16-bit
	assert(vidstart <= UINT16_MAX);
	assert(line <= UINT16_MAX);

	static uint8_t temp[643] = {0};
	const uint8_t *base = vga.tandy.draw_base + ((line & vga.tandy.line_mask)
	                                             << vga.tandy.line_shift);

	auto read_cga16_offset = [=](uint16_t offset) -> uint8_t {
		const auto index = static_cast<uint16_t>(vidstart) + offset;
		constexpr auto index_mask = static_cast<uint16_t>(8 * 1024 - 1);
		return base[index & index_mask];
	};

	// There are 640 hdots in each line of the screen.
	// The color of an even hdot always depends on only 4 bits of video RAM.
	// The color of an odd hdot depends on 4 bits of video RAM in
	// 1-hdot-per-pixel modes and 6 bits of video RAM in 2-hdot-per-pixel
	// modes. We always assume 6 and use duplicate palette entries in
	// 1-hdot-per-pixel modes so that we can use the same routine for all
	// composite modes.
	temp[1] = (read_cga16_offset(0) >> 6) & 3;

	for (uint16_t x = 2; x < 640; x += 2) {
		temp[x] = (temp[x - 1] & 0xf);
		temp[x + 1] = (temp[x] << 2) |
		              (((read_cga16_offset(x >> 3)) >> (6 - (x & 6))) & 3);
	}
	temp[640] = temp[639] & 0xf;
	temp[641] = temp[640] << 2;
	temp[642] = temp[641] & 0xf;

	for (uint32_t i = 2, j = 0, x = 0; x < vga.draw.blocks * 2; i += 4, ++j, ++x) {
		constexpr uint32_t foundation = 0xc0708030; // colors are OR'd
		                                            // on top of this
		const uint32_t pixel = temp[i] | (temp[i + 1] << 8) | (temp[i + 2] << 16) | (temp[i + 3] << 24);
		write_unaligned_uint32_at(TempLine, j, foundation | pixel);
	}
	return TempLine;
}

static uint8_t byte_clamp(int v)
{
	v >>= 13;
	return v < 0 ? 0u : (v > 255 ? 255u : static_cast<uint8_t>(v));
}

static uint8_t *Composite_Process(uint8_t border, uint32_t blocks, bool doublewidth)
{
	static int temp[SCALER_MAXWIDTH + 10] = {0};
	static int atemp[SCALER_MAXWIDTH + 2] = {0};
	static int btemp[SCALER_MAXWIDTH + 2] = {0};

	int w = blocks * 4;

	if (doublewidth) {
		uint8_t *source = TempLine + w - 1;
		uint8_t *dest = TempLine + w * 2 - 2;
		for (int x = 0; x < w; ++x) {
			*dest = *source;
			*(dest + 1) = *source;
			--source;
			dest -= 2;
		}
		blocks *= 2;
		w *= 2;
	}

	// Simulate CGA composite output
	int *o = temp;
	auto push_pixel = [&o](const int v) {
		*o = v;
		++o;
	};

	uint8_t *rgbi = TempLine;
	int *b = &CGA_Composite_Table[border * 68];
	for (int x = 0; x < 4; ++x)
		push_pixel(b[(x + 3) & 3]);
	push_pixel(CGA_Composite_Table[(border << 6) | ((*rgbi) << 2) | 3]);
	for (int x = 0; x < w - 1; ++x) {
		push_pixel(CGA_Composite_Table[(rgbi[0] << 6) | (rgbi[1] << 2) | (x & 3)]);
		++rgbi;
	}
	push_pixel(CGA_Composite_Table[((*rgbi) << 6) | (border << 2) | 3]);
	for (int x = 0; x < 5; ++x)
		push_pixel(b[x & 3]);

	if ((vga.tandy.mode_control & 4) != 0) {
		// Decode
		int *i = temp + 5;
		uint16_t idx = 0;
		for (uint32_t x = 0; x < blocks * 4; ++x) {
			int c = (i[0] + i[0]) << 3;
			int d = (i[-1] + i[1]) << 3;
			int y = ((c + d) << 8) + vga.sharpness * (c - d);
			++i;
			write_unaligned_uint32_at(TempLine, idx++,
			                          byte_clamp(y) * 0x10101);
		}
	} else {
		// Store chroma
		int *i = temp + 4;
		int *ap = atemp + 1;
		int *bp = btemp + 1;
		for (int x = -1; x < w + 1; ++x) {
			ap[x] = i[-4] - left_shift_signed(i[-2] - i[0] + i[2], 1) + i[4];
			bp[x] = left_shift_signed(i[-3] - i[-1] + i[1] - i[3], 1);
			++i;
		}

		// Decode
		i = temp + 5;
		i[-1] = (i[-1] << 3) - ap[-1];
		i[0] = (i[0] << 3) - ap[0];

		uint16_t idx = 0;
		auto COMPOSITE_CONVERT = [&](const int I, const int Q) {
			i[1] = (i[1] << 3) - ap[1];
			const int c = i[0] + i[0];
			const int d = i[-1] + i[1];
			const int y = left_shift_signed(c + d, 8) + vga.sharpness * (c - d);
			const int rr = y + vga.ri * (I) + vga.rq * (Q);
			const int gg = y + vga.gi * (I) + vga.gq * (Q);
			const int bb = y + vga.bi * (I) + vga.bq * (Q);
			++i;
			++ap;
			++bp;
			const auto srgb = (byte_clamp(rr) << 16) |
			                  (byte_clamp(gg) << 8) | byte_clamp(bb);
			write_unaligned_uint32_at(TempLine, idx++, srgb);
		};

		for (uint32_t x = 0; x < blocks; ++x) {
			COMPOSITE_CONVERT(ap[0], bp[0]);
			COMPOSITE_CONVERT(-bp[0], ap[0]);
			COMPOSITE_CONVERT(-ap[0], -bp[0]);
			COMPOSITE_CONVERT(bp[0], -ap[0]);
		}
	}
	return TempLine;
}

static uint8_t *VGA_TEXT_Draw_Line(Bitu vidstart, Bitu line);

static uint8_t *VGA_CGA_TEXT_Composite_Draw_Line(Bitu vidstart, Bitu line)
{
	VGA_TEXT_Draw_Line(vidstart, line);
	return Composite_Process(vga.tandy.color_select & 0x0f, vga.draw.blocks * 2,
	                         (vga.tandy.mode_control & 0x1) == 0);
}

static uint8_t *VGA_Draw_CGA2_Composite_Line(Bitu vidstart, Bitu line)
{
	VGA_Draw_1BPP_Line(vidstart, line);
	return Composite_Process(0, vga.draw.blocks * 2, false);
}

static uint8_t *VGA_Draw_CGA4_Composite_Line(Bitu vidstart, Bitu line)
{
	VGA_Draw_2BPP_Line(vidstart, line);
	return Composite_Process(vga.tandy.color_select & 0x0f, vga.draw.blocks, true);
}

static uint8_t * VGA_Draw_4BPP_Line(Bitu vidstart, Bitu line) {
	const uint8_t *base = vga.tandy.draw_base + ((line & vga.tandy.line_mask) << vga.tandy.line_shift);
	uint8_t* draw=TempLine;
	Bitu end = vga.draw.blocks*2;
	while(end) {
		uint8_t byte = base[vidstart & vga.tandy.addr_mask];
		*draw++=vga.attr.palette[byte >> 4];
		*draw++=vga.attr.palette[byte & 0x0f];
		++vidstart;
		--end;
	}
	return TempLine;
}

static uint8_t * VGA_Draw_4BPP_Line_Double(Bitu vidstart, Bitu line) {
	const uint8_t *base = vga.tandy.draw_base + ((line & vga.tandy.line_mask) << vga.tandy.line_shift);
	uint8_t* draw=TempLine;
	Bitu end = vga.draw.blocks;
	while(end) {
		uint8_t byte = base[vidstart & vga.tandy.addr_mask];
		uint8_t data = vga.attr.palette[byte >> 4];
		*draw++ = data; *draw++ = data;
		data = vga.attr.palette[byte & 0x0f];
		*draw++ = data; *draw++ = data;
		++vidstart;
		--end;
	}
	return TempLine;
}

#ifdef VGA_KEEP_CHANGES
static uint8_t * VGA_Draw_Changes_Line(Bitu vidstart, Bitu line) {
	Bitu checkMask = vga.changes.checkMask;
	uint8_t *map = vga.changes.map;
	Bitu start = (vidstart >> VGA_CHANGE_SHIFT);
	Bitu end = ((vidstart + vga.draw.line_length ) >> VGA_CHANGE_SHIFT);
	for (; start <= end; ++start) {
		if ( map[start] & checkMask ) {
			Bitu offset = vidstart & vga.draw.linear_mask;
			if (vga.draw.linear_mask-offset < vga.draw.line_length)
				memcpy(vga.draw.linear_base+vga.draw.linear_mask+1, vga.draw.linear_base, vga.draw.line_length);
			uint8_t *ret = &vga.draw.linear_base[ offset ];
#if !defined(C_UNALIGNED_MEMORY)
			if (GCC_UNLIKELY( ((Bitu)ret) & (sizeof(Bitu)-1)) ) {
				memcpy( TempLine, ret, vga.draw.line_length );
				return TempLine;
			}
#endif
			return ret;
		}
	}
//	memset( TempLine, 0x30, vga.changes.lineWidth );
//	return TempLine;
	return 0;
}

#endif

static uint8_t * VGA_Draw_Linear_Line(Bitu vidstart, Bitu /*line*/) {
	Bitu offset = vidstart & vga.draw.linear_mask;
	uint8_t* ret = &vga.draw.linear_base[offset];

	// in case (vga.draw.line_length + offset) has bits set that
	// are not set in the mask: ((x|y)!=y) equals (x&~y)
	if (GCC_UNLIKELY((vga.draw.line_length + offset)& ~vga.draw.linear_mask)) {
		// this happens, if at all, only once per frame (1 of 480 lines)
		// in some obscure games
		Bitu end = (offset + vga.draw.line_length) & vga.draw.linear_mask;

		// assuming lines not longer than 4096 pixels
		Bitu wrapped_len = end & 0xFFF;
		Bitu unwrapped_len = vga.draw.line_length-wrapped_len;

		// unwrapped chunk: to top of memory block
		memcpy(TempLine, &vga.draw.linear_base[offset], unwrapped_len);
		// wrapped chunk: from base of memory block
		memcpy(&TempLine[unwrapped_len], vga.draw.linear_base, wrapped_len);

		ret = TempLine;
	}

#if !defined(C_UNALIGNED_MEMORY)
	if (GCC_UNLIKELY( ((Bitu)ret) & (sizeof(Bitu)-1)) ) {
		memcpy( TempLine, ret, vga.draw.line_length );
		return TempLine;
	}
#endif
	return ret;
}

static uint8_t* draw_unwrapped_line_from_dac_palette(Bitu vidstart,
                                                     [[maybe_unused]] const Bitu line = 0)
{
	// Quick references
	static constexpr auto palette_map        = vga.dac.palette_map;
	static constexpr uint8_t bytes_per_pixel = sizeof(palette_map[0]);
	const auto linear_mask                   = vga.draw.linear_mask;
	const auto linear_addr                   = vga.draw.linear_base;

	// Video mode-specific line variables
	const auto pixels_in_line = static_cast<uint16_t>(vga.draw.line_length /
	                                                  bytes_per_pixel);
	const auto video_end = vidstart + pixels_in_line;

	// The line address is where the RGB888 palettized pixel is written.
	// It's incremented forward per pixel.
	auto line_addr = reinterpret_cast<uint32_t*>(TempLine);

	auto linear_pos = vidstart;

	// Draw in batches of four to let the host pipeline deeper.
	constexpr auto num_repeats = 4;
	assert(pixels_in_line % num_repeats == 0);

	// This function typically runs on 640+-wide lines and is a rendering
	// bottleneck.
	while (linear_pos < video_end) {
		auto repeats = num_repeats;
		while (repeats--) {
			const auto masked_pos    = linear_pos++ & linear_mask;
			const auto palette_index = *(linear_addr + masked_pos);
			*line_addr++ = *(palette_map + palette_index);
		}
	}

	return TempLine;
}

static uint8_t* draw_linear_line_from_dac_palette(Bitu vidstart, Bitu /*line*/)
{
	const auto offset                 = vidstart & vga.draw.linear_mask;
	constexpr auto palette_map        = vga.dac.palette_map;
	constexpr uint8_t bytes_per_pixel = sizeof(palette_map[0]);

	// The line address is where the RGB888 palettized pixel is written.
	// It's incremented forward per pixel.
	auto line_addr = TempLine;

	// The palette index iterator is used to lookup the DAC palette colour.
	// It starts at the current VGA line's offset and is incremented per
	// pixel.
	auto palette_index_it = vga.draw.linear_base + offset;

	// Pixels remaining starts as the total pixels in this current line and
	// is decremented per pixel rendered. It acts as a lower-bound cutoff
	// regardless of how long the wrapped and unwrapped regions are.
	auto pixels_remaining = check_cast<uint16_t>(vga.draw.line_length /
	                                             bytes_per_pixel);

	// see VGA_Draw_Linear_Line
	if (GCC_UNLIKELY((vga.draw.line_length + offset) & ~vga.draw.linear_mask)) {
		// Note: To exercise these wrapped scenarios, run:
		// 1. Dangerous Dave: jump on the tree at the start.
		// 2. Commander Keen 4: move to left of the first hill on stage 1.

		const auto end = (vga.draw.line_length + offset) &
		                 vga.draw.linear_mask;

		// assuming lines not longer than 4096 pixels
		const auto wrapped_len   = static_cast<uint16_t>(end & 0xFFF);
		const auto unwrapped_len = check_cast<uint16_t>(
		        vga.draw.line_length - wrapped_len);

		// unwrapped chunk: to top of memory block
		auto palette_index_end = palette_index_it +
		                         std::min(unwrapped_len, pixels_remaining);
		while (palette_index_it != palette_index_end) {
			memcpy(line_addr,
			       palette_map + *palette_index_it++,
			       bytes_per_pixel);
			line_addr += bytes_per_pixel;
			--pixels_remaining;
		}

		// wrapped chunk: from the base of the memory block
		palette_index_it  = vga.draw.linear_base;
		palette_index_end = palette_index_it +
		                    std::min(wrapped_len, pixels_remaining);
		while (palette_index_it != palette_index_end) {
			memcpy(line_addr,
			       palette_map + *palette_index_it++,
			       bytes_per_pixel);
			line_addr += bytes_per_pixel;
			--pixels_remaining;
		}

	} else {
		auto palette_index_end = palette_index_it + pixels_remaining;
		while (palette_index_it != palette_index_end) {
			memcpy(line_addr,
			       palette_map + *palette_index_it++,
			       bytes_per_pixel);
			line_addr += bytes_per_pixel;
		}
	}
	return TempLine;
}

//Test version, might as well keep it
/* static uint8_t * VGA_Draw_Chain_Line(Bitu vidstart, Bitu line) {
	Bitu i = 0;
	for ( i = 0; i < vga.draw.width;i++ ) {
		Bitu addr = vidstart + i;
		TempLine[i] = vga.mem.linear[((addr&~3)<<2)+(addr&3)];
	}
	return TempLine;
} */

enum CursorOp : uint8_t {
	Foreground  = 0b10,
	Background  = 0b00,
	Transparent = 0b01,
	Invert      = 0b11,
};

static uint8_t* draw_unwrapped_line_from_dac_palette_with_hwcursor(Bitu vidstart, Bitu /*line*/)
{
	// Draw the underlying line without the cursor
	const auto line_addr = reinterpret_cast<uint32_t*>(
	        draw_unwrapped_line_from_dac_palette(vidstart));

	// Quick references to hardware cursor properties
	const auto& cursor = vga.s3.hgc;
	const auto line_at_y = (vidstart - (vga.config.real_start << 2)) / vga.draw.width;

	// Draw mouse cursor
	// ~~~~~~~~~~~~~~~~~
	// The cursor is a 64x64 pattern which is shifted (inside the 64x64 mouse
	// cursor space) to the right by posx pixels and up by posy pixels.
	//
	// This is used when the mouse cursor partially leaves the screen. It is
	// arranged as bitmap of 16bits of bit A followed by 16bits of bit B, each AB
	// bits corresponding to a cursor pixel. The whole map is 8kB in size.

	constexpr auto bitmap_width_bits   = 64;
	constexpr auto bitmap_last_y_index = 63u;

	// Is the mouse cursor pattern on this line?
	if (cursor.posx >= vga.draw.width || line_at_y < cursor.originy ||
	    line_at_y > (cursor.originy + (bitmap_last_y_index - cursor.posy))) {
		return reinterpret_cast<uint8_t*>(line_addr);
	}

	using namespace bit;
	// the index of the bit inside the cursor bitmap we start at:
	const auto source_start_bit = ((line_at_y - cursor.originy) + cursor.posy) *
	                                      bitmap_width_bits + cursor.posx;
	const auto cursor_start_bit = source_start_bit & 0x7;
	uint8_t cursor_bit = literals::b7 >> cursor_start_bit;

	// Convert to video memory addr and bit index
	// start adjusted to the pattern structure (thus shift address by 2
	// instead of 3) Need to get rid of the third bit, so "/8 *2" becomes
	// ">> 2 & ~1"
	auto mem_start = ((source_start_bit >> 2) & ~1u) +
	                 (static_cast<uint32_t>(cursor.startaddr) << 10);

	// Stay at the right position in the pattern
	if (mem_start & 0x2) {
		--mem_start;
	}
	const auto mem_end = mem_start + ((bitmap_width_bits - cursor.posx) >> 2);

	constexpr uint8_t mem_delta[] = {1, 3};

	// Iterated for all bitmap positions
	auto cursor_addr = line_addr + cursor.originx;

	const auto fg_colour = *(vga.dac.palette_map + *cursor.forestack);
	const auto bg_colour = *(vga.dac.palette_map + *cursor.backstack);

	// for each byte of cursor data
	for (auto m = mem_start; m < mem_end; m += mem_delta[m & 1]) {
		const auto bits_a = vga.mem.linear[m];
		const auto bits_b = vga.mem.linear[m + 2];

		while (cursor_bit != 0) {
			uint8_t op = {};
			set_to(op, literals::b0, is(bits_a, cursor_bit));
			set_to(op, literals::b1, is(bits_b, cursor_bit));

			switch (static_cast<CursorOp>(op)) {
			case CursorOp::Foreground: *cursor_addr = fg_colour; break;
			case CursorOp::Background: *cursor_addr = bg_colour; break;
			case CursorOp::Invert: flip_all(*cursor_addr); break;
			case CursorOp::Transparent: break;
			};
			cursor_addr++;
			cursor_bit >>= 1;
		}
		cursor_bit = literals::b7;
	}
	return reinterpret_cast<uint8_t*>(line_addr);
}

static uint8_t * VGA_Draw_LIN16_Line_HWMouse(Bitu vidstart, Bitu /*line*/) {
	if (!svga.hardware_cursor_active || !svga.hardware_cursor_active())
		return &vga.mem.linear[vidstart];

	Bitu lineat = ((vidstart-(vga.config.real_start<<2)) >> 1) / vga.draw.width;
	if ((vga.s3.hgc.posx >= vga.draw.width) ||
		(lineat < vga.s3.hgc.originy) ||
		(lineat > (vga.s3.hgc.originy + (63U-vga.s3.hgc.posy))) ) {
		return &vga.mem.linear[vidstart];
	} else {
		memcpy(TempLine, &vga.mem.linear[ vidstart ], vga.draw.width*2);
		Bitu sourceStartBit = ((lineat - vga.s3.hgc.originy) + vga.s3.hgc.posy)*64 + vga.s3.hgc.posx;
 		Bitu cursorMemStart = ((sourceStartBit >> 2)& ~1) + (((uint32_t)vga.s3.hgc.startaddr) << 10);
		Bitu cursorStartBit = sourceStartBit & 0x7;
		if (cursorMemStart & 0x2) cursorMemStart--;
		Bitu cursorMemEnd = cursorMemStart + ((64-vga.s3.hgc.posx) >> 2);

		uint16_t i = vga.s3.hgc.originx;
		for (Bitu m = cursorMemStart; m < cursorMemEnd;
		     (m & 1) ? (m += 3) : ++m) {
			// for each byte of cursor data
			uint8_t bitsA = vga.mem.linear[m];
			uint8_t bitsB = vga.mem.linear[m+2];
			for (uint8_t bit=(0x80 >> cursorStartBit); bit != 0; bit >>= 1) {
				// for each bit
				cursorStartBit=0;
				if (bitsA&bit) {
					// byte order doesn't matter here as all bits get flipped
					if (bitsB & bit) {
						const auto xat = read_unaligned_uint16_at(TempLine, i);
						write_unaligned_uint16_at(TempLine, i, xat ^ 0xffff);
					}
					// else Transparent
				} else if (bitsB & bit) {
					// Source as well as destination are uint8_t arrays,
					// so this should work out endian-wise?
					const auto fore = read_unaligned_uint16(vga.s3.hgc.forestack);
					write_unaligned_uint16_at(TempLine, i, fore);
				} else {
					const auto back = read_unaligned_uint16(vga.s3.hgc.backstack);
					write_unaligned_uint16_at(TempLine, i, back);
				}
				++i;
			}
		}
		return TempLine;
	}
}

static uint8_t * VGA_Draw_LIN32_Line_HWMouse(Bitu vidstart, Bitu /*line*/) {
	if (!svga.hardware_cursor_active || !svga.hardware_cursor_active())
		return &vga.mem.linear[vidstart];

	Bitu lineat = ((vidstart-(vga.config.real_start<<2)) >> 2) / vga.draw.width;
	if ((vga.s3.hgc.posx >= vga.draw.width) ||
		(lineat < vga.s3.hgc.originy) ||
		(lineat > (vga.s3.hgc.originy + (63U-vga.s3.hgc.posy))) ) {
		return &vga.mem.linear[ vidstart ];
	} else {
		memcpy(TempLine, &vga.mem.linear[ vidstart ], vga.draw.width*4);
		Bitu sourceStartBit = ((lineat - vga.s3.hgc.originy) + vga.s3.hgc.posy)*64 + vga.s3.hgc.posx;
		Bitu cursorMemStart = ((sourceStartBit >> 2)& ~1) + (((uint32_t)vga.s3.hgc.startaddr) << 10);
		Bitu cursorStartBit = sourceStartBit & 0x7;
		if (cursorMemStart & 0x2)
			--cursorMemStart;
		Bitu cursorMemEnd = cursorMemStart + ((64-vga.s3.hgc.posx) >> 2);
		uint16_t i = vga.s3.hgc.originx;
		for (Bitu m = cursorMemStart; m < cursorMemEnd;
		     (m & 1) ? (m += 3) : ++m) {
			// for each byte of cursor data
			uint8_t bitsA = vga.mem.linear[m];
			uint8_t bitsB = vga.mem.linear[m+2];
			for (uint8_t bit=(0x80 >> cursorStartBit); bit != 0; bit >>= 1) { // for each bit
				cursorStartBit=0;
				if (bitsA&bit) {
					if (bitsB & bit) {
						const auto xat = read_unaligned_uint32_at(TempLine, i);
						write_unaligned_uint32_at(TempLine, i, xat ^ 0xffff);
					}
					// else Transparent
				} else if (bitsB & bit) {
					const auto fore = read_unaligned_uint32(vga.s3.hgc.forestack);
					write_unaligned_uint32_at(TempLine, i, fore);
				} else {
					const auto back = read_unaligned_uint32(vga.s3.hgc.backstack);
					write_unaligned_uint32_at(TempLine, i, back);
				}
				++i;
			}
		}
		return TempLine;
	}
}

static const uint8_t* VGA_Text_Memwrap(Bitu vidstart) {
	vidstart = vidstart & vga.draw.linear_mask;
	Bitu line_end = 2 * vga.draw.blocks;
	if (GCC_UNLIKELY((vidstart + line_end) > vga.draw.linear_mask)) {
		// wrapping in this line
		Bitu break_pos = (vga.draw.linear_mask - vidstart) + 1;
		// need a temporary storage - TempLine/2 is ok for a bit more than 132 columns
		memcpy(&TempLine[templine_buffer.size() / 2],
		       &vga.tandy.draw_base[vidstart],
		       break_pos);
		memcpy(&TempLine[templine_buffer.size() / 2 + break_pos],
		       &vga.tandy.draw_base[0],
		       line_end - break_pos);
		return &TempLine[templine_buffer.size() / 2];
	} else {
		return &vga.tandy.draw_base[vidstart];
	}
}

static bool SkipCursor(Bitu vidstart, Bitu line)
{
	return !vga.draw.cursor.enabled || !(vga.draw.cursor.count & 0x10) ||
	       (line < vga.draw.cursor.sline) || (line > vga.draw.cursor.eline) ||
	       (vga.draw.cursor.address < vidstart);
}

static uint32_t FontMask[2]={0xffffffff,0x0};
static uint8_t *VGA_TEXT_Draw_Line(Bitu vidstart, Bitu line)
{
	uint16_t i = 0;
	const uint8_t* vidmem = VGA_Text_Memwrap(vidstart);
	for (Bitu cx = 0; cx < vga.draw.blocks; ++cx) {
		Bitu chr=vidmem[cx*2];
		Bitu col=vidmem[cx*2+1];
		Bitu font=vga.draw.font_tables[(col >> 3)&1][chr*32+line];
		uint32_t mask1=TXT_Font_Table[font>>4] & FontMask[col >> 7];
		uint32_t mask2=TXT_Font_Table[font&0xf] & FontMask[col >> 7];
		uint32_t fg=TXT_FG_Table[col&0xf];
		uint32_t bg=TXT_BG_Table[col>>4];
		write_unaligned_uint32_at(TempLine, i++, (fg & mask1) | (bg & ~mask1));
		write_unaligned_uint32_at(TempLine, i++, (fg & mask2) | (bg & ~mask2));
	}
	if (SkipCursor(vidstart, line))
		return TempLine;
	const Bitu font_addr = (vga.draw.cursor.address - vidstart) >> 1;
	if (font_addr < vga.draw.blocks) {
		uint32_t *draw = (uint32_t *)&TempLine[font_addr * 8];
		uint32_t att=TXT_FG_Table[vga.tandy.draw_base[vga.draw.cursor.address+1]&0xf];
		*draw++ = att;
		*draw++ = att;
	}
	return TempLine;
}

static uint8_t *VGA_TEXT_Herc_Draw_Line(Bitu vidstart, Bitu line)
{
	uint16_t i = 0;
	const uint8_t* vidmem = VGA_Text_Memwrap(vidstart);

	for (Bitu cx = 0; cx < vga.draw.blocks; ++cx) {
		Bitu chr=vidmem[cx*2];
		Bitu attrib=vidmem[cx*2+1];
		if (!(attrib&0x77)) {
			// 00h, 80h, 08h, 88h produce black space
			write_unaligned_uint32_at(TempLine, i++, 0);
			write_unaligned_uint32_at(TempLine, i++, 0);
		} else {
			uint32_t bg, fg;
			bool underline=false;
			if ((attrib&0x77)==0x70) {
				bg = TXT_BG_Table[0x7];
				if (attrib&0x8) fg = TXT_FG_Table[0xf];
				else fg = TXT_FG_Table[0x0];
			} else {
				if (((Bitu)(vga.crtc.underline_location&0x1f)==line) && ((attrib&0x77)==0x1)) underline=true;
				bg = TXT_BG_Table[0x0];
				if (attrib&0x8) fg = TXT_FG_Table[0xf];
				else fg = TXT_FG_Table[0x7];
			}
			uint32_t mask1, mask2;
			if (GCC_UNLIKELY(underline)) mask1 = mask2 = FontMask[attrib >> 7];
			else {
				Bitu font=vga.draw.font_tables[0][chr*32+line];
				mask1=TXT_Font_Table[font>>4] & FontMask[attrib >> 7]; // blinking
				mask2=TXT_Font_Table[font&0xf] & FontMask[attrib >> 7];
			}
			write_unaligned_uint32_at(TempLine, i++, (fg & mask1) | (bg & ~mask1));
			write_unaligned_uint32_at(TempLine, i++, (fg & mask2) | (bg & ~mask2));
		}
	}
	if (SkipCursor(vidstart, line))
		return TempLine;
	const Bitu font_addr = (vga.draw.cursor.address - vidstart) >> 1;
	if (font_addr < vga.draw.blocks) {
		uint32_t *draw = (uint32_t *)&TempLine[font_addr * 8];
		uint8_t attr = vga.tandy.draw_base[vga.draw.cursor.address+1];
		uint32_t cg;
		if (attr&0x8) {
			cg = TXT_FG_Table[0xf];
		} else if ((attr&0x77)==0x70) {
			cg = TXT_FG_Table[0x0];
		} else {
			cg = TXT_FG_Table[0x7];
		}
		*draw++ = cg;
		*draw++ = cg;
	}
	return TempLine;
}
// combined 8/9-dot wide text mode line drawing function
static uint8_t* draw_text_line_from_dac_palette(Bitu vidstart, Bitu line)
{
	// pointer to chars+attribs
	const uint8_t* vidmem  = VGA_Text_Memwrap(vidstart);
	const auto palette_map = vga.dac.palette_map;

	auto blocks = vga.draw.blocks;
	if (vga.draw.panning) {
		++blocks; // if the text is panned part of an
		          // additional character becomes visible
	}

	// The first write-index into the draw buffer. Increasing this shifts
	// the console text right (and vice-versa)
	const uint16_t draw_idx_start = 8 + vga.draw.panning;

	// This holds the to-be-written pixel offset, and is incremented per
	// pixel and also per character block.
	auto draw_idx = draw_idx_start;

	while (blocks--) { // for each character in the line
		const auto chr  = *vidmem++;
		const auto attr = *vidmem++;
		// the font pattern
		uint16_t font = vga.draw.font_tables[(attr >> 3) & 1][(chr << 5) + line];

		uint8_t bg_palette_idx = attr >> 4;
		// if blinking is enabled bit7 is not mapped to attributes
		if (vga.draw.blinking) {
			bg_palette_idx &= ~0x8;
		}
		// choose foreground color if blinking not set for this cell or
		// blink on
		const uint8_t fg_palette_idx = (vga.draw.blink || (attr & 0x80) == 0)
		                                     ? (attr & 0xf)
		                                     : bg_palette_idx;

		// underline: all foreground [freevga: 0x77, previous 0x7]
		if (GCC_UNLIKELY(((attr&0x77) == 0x01) &&
			(vga.crtc.underline_location&0x1f)==line))
			bg_palette_idx = fg_palette_idx;

		// The font's bits will indicate which color is used per pixel
		const auto fg_colour = palette_map[fg_palette_idx];
		const auto bg_colour = palette_map[bg_palette_idx];

		if (vga.seq.clocking_mode.is_eight_dot_mode) {
			for (auto n = 0; n < 8; ++n) {
				const auto color = (font & 0x80) ? fg_colour
				                                 : bg_colour;
				write_unaligned_uint32_at(TempLine, draw_idx++, color);
				font <<= 1;
			}
		} else {
			font <<= 1; // 9 pixels
			// extend to the 9th pixel if needed
			if ((font&0x2) && (vga.attr.mode_control&0x04) &&
				(chr>=0xc0) && (chr<=0xdf)) font |= 1;
			for (auto n = 0; n < 9; ++n) {
				const auto color = (font & 0x100) ? fg_colour
				                                  : bg_colour;
				write_unaligned_uint32_at(TempLine, draw_idx++, color);
				font <<= 1;
			}
		}
	}
	// draw the text mode cursor if needed
	if (!SkipCursor(vidstart, line)) {
		// the adress of the attribute that makes up the cell the cursor is in
		const auto attr_addr = check_cast<uint16_t>(
		        (vga.draw.cursor.address - vidstart) >> 1);
		if (attr_addr < vga.draw.blocks) {
			const auto fg_palette_idx =
			        vga.tandy.draw_base[vga.draw.cursor.address + 1] & 0xf;
			const auto fg_colour = palette_map[fg_palette_idx];

			constexpr auto bytes_per_pixel = sizeof(fg_colour);

			// The cursor block's byte-offset into the rendering buffer.
			const auto cursor_draw_offset = check_cast<uint16_t>(
			        attr_addr * vga.draw.pixels_per_character * bytes_per_pixel);

			auto draw_addr = &TempLine[cursor_draw_offset];

			draw_idx = draw_idx_start;
			for (uint8_t n = 0; n < 8; ++n) {
				write_unaligned_uint32_at(draw_addr, draw_idx++, fg_colour);
			}
		}
	}
	return TempLine + 32;
}

#ifdef VGA_KEEP_CHANGES
static inline void VGA_ChangesEnd(void ) {
	if ( vga.changes.active ) {
//		vga.changes.active = false;
		Bitu end = vga.draw.address >> VGA_CHANGE_SHIFT;
		Bitu total = 4 + end - vga.changes.start;
		uint32_t clearMask = vga.changes.clearMask;
		total >>= 2;
		uint32_t *clear = (uint32_t *)&vga.changes.map[  vga.changes.start & ~3 ];
		while ( total-- ) {
			clear[0] &= clearMask;
			++clear;
		}
	}
}
#endif


static void VGA_ProcessSplit() {
	if (vga.attr.mode_control&0x20) {
		vga.draw.address=0;
		// reset panning to 0 here so we don't have to check for
		// it in the character draw functions. It will be set back
		// to its proper value in v-retrace
		vga.draw.panning=0;
	} else {
		// In text mode only the characters are shifted by panning, not the address;
		// this is done in the text line draw function.
		vga.draw.address = vga.draw.byte_panning_shift*vga.draw.bytes_skip;
		if ((vga.mode!=M_TEXT)&&(machine!=MCH_EGA)) vga.draw.address += vga.draw.panning;
	}
	vga.draw.address_line=0;
}

static uint16_t from_rgb_888_to_565(const uint32_t rgb888)
{
	const auto rgb565 = ((rgb888 & 0xf80000) >> 8) +
	                    ((rgb888 & 0xfc00) >> 5) + ((rgb888 & 0xf8) >> 3);

	return check_cast<uint16_t>(rgb565);
}

static uint8_t bg_color_index = 0; // screen-off black index
static void VGA_DrawSingleLine(uint32_t /*blah*/)
{
	if (GCC_UNLIKELY(vga.attr.disabled)) {
		switch(machine) {
		case MCH_PCJR:
			// Displays the border color when screen is disabled
			bg_color_index = vga.tandy.border_color;
			break;
		case MCH_TANDY:
			// Either the PCJr way or the CGA way
			if (vga.tandy.gfx_control& 0x4) {
				bg_color_index = vga.tandy.border_color;
			} else if (vga.mode==M_TANDY4)
				bg_color_index = vga.attr.palette[0];
			else bg_color_index = 0;
			break;
		case MCH_CGA:
			// the background color
			bg_color_index = vga.attr.overscan_color;
			break;
		case MCH_EGA:
		case MCH_VGA:
			// DoWhackaDo, Alien Carnage, TV sports Football
			// when disabled by attribute index bit 5:
			//  ET3000, ET4000, Paradise display the border color
			//  S3 displays the content of the currently selected attribute register
			// when disabled by sequencer the screen is black "257th color"

			// the DAC table may not match the bits of the overscan register
			// so use black for this case too...
			//if (vga.attr.disabled& 2) {
			if (constexpr uint32_t black_rgb888 = 0;
			    vga.dac.palette_map[bg_color_index] != black_rgb888) {
				// check some assumptions about the palette map
				constexpr auto palette_map_len = ARRAY_LEN(
				        vga.dac.palette_map);
				static_assert(palette_map_len == 256,
				              "The code below assumes the table is 256 elements long");

				for (uint16_t i = 0; i < palette_map_len; ++i)
					if (vga.dac.palette_map[i] == black_rgb888) {
						bg_color_index = static_cast<uint8_t>(i);
						break;
					}
			}
			//} else
            //    bg_color_index = vga.attr.overscan_color;
			break;
		default:
			bg_color_index = 0;
			break;
		}
		if (vga.draw.bpp==8) {
			std::fill(templine_buffer.begin(),
			          templine_buffer.end(),
			          bg_color_index);
		} else if (vga.draw.bpp == 16) {
			const auto background_color = from_rgb_888_to_565(
			        vga.dac.palette_map[bg_color_index]);
			const auto line_length = templine_buffer.size() / sizeof(uint16_t);
			size_t i = 0;
			while (i < line_length) {
				write_unaligned_uint16_at(TempLine, i++, background_color);
			}
		} else if (vga.draw.bpp == 32) {
			const auto background_color = vga.dac.palette_map[bg_color_index];
			const auto line_length = templine_buffer.size() / sizeof(uint32_t);
			size_t i = 0;
			while (i < line_length) {
				write_unaligned_uint32_at(TempLine, i++, background_color);
			}
		}
		RENDER_DrawLine(TempLine);
	} else {
		uint8_t * data=VGA_DrawLine( vga.draw.address, vga.draw.address_line );
		RENDER_DrawLine(data);
	}

	++vga.draw.address_line;
	if (vga.draw.address_line>=vga.draw.address_line_total) {
		vga.draw.address_line=0;
		vga.draw.address+=vga.draw.address_add;
	}
	++vga.draw.lines_done;
	if (vga.draw.split_line==vga.draw.lines_done) VGA_ProcessSplit();
	if (vga.draw.lines_done < vga.draw.lines_total) {
		PIC_AddEvent(VGA_DrawSingleLine, vga.draw.delay.per_line_ms);
	} else RENDER_EndUpdate(false);
}

static void VGA_DrawEGASingleLine(uint32_t /*blah*/)
{
	if (GCC_UNLIKELY(vga.attr.disabled)) {
		std::fill(templine_buffer.begin(), templine_buffer.end(), 0);
		RENDER_DrawLine(TempLine);
	} else {
		Bitu address = vga.draw.address;
		if (vga.mode!=M_TEXT) address += vga.draw.panning;
		uint8_t * data=VGA_DrawLine(address, vga.draw.address_line );
		RENDER_DrawLine(data);
	}

	++vga.draw.address_line;
	if (vga.draw.address_line>=vga.draw.address_line_total) {
		vga.draw.address_line=0;
		vga.draw.address+=vga.draw.address_add;
	}
	++vga.draw.lines_done;
	if (vga.draw.split_line==vga.draw.lines_done) VGA_ProcessSplit();
	if (vga.draw.lines_done < vga.draw.lines_total) {
		PIC_AddEvent(VGA_DrawEGASingleLine, vga.draw.delay.per_line_ms);
	} else RENDER_EndUpdate(false);
}

static void VGA_DrawPart(uint32_t lines)
{
	while (lines--) {
		uint8_t * data=VGA_DrawLine( vga.draw.address, vga.draw.address_line );
		RENDER_DrawLine(data);
		++vga.draw.address_line;
		if (vga.draw.address_line>=vga.draw.address_line_total) {
			vga.draw.address_line=0;
			vga.draw.address+=vga.draw.address_add;
		}
		++vga.draw.lines_done;
		if (vga.draw.split_line==vga.draw.lines_done) {
#ifdef VGA_KEEP_CHANGES
			VGA_ChangesEnd( );
#endif
			VGA_ProcessSplit();
#ifdef VGA_KEEP_CHANGES
			vga.changes.start = vga.draw.address >> VGA_CHANGE_SHIFT;
#endif
		}
	}
	if (--vga.draw.parts_left) {
		PIC_AddEvent(VGA_DrawPart, vga.draw.delay.parts,
		             (vga.draw.parts_left != 1)
		                     ? vga.draw.parts_lines
		                     : (vga.draw.lines_total - vga.draw.lines_done));
	} else {
#ifdef VGA_KEEP_CHANGES
		VGA_ChangesEnd();
#endif
		RENDER_EndUpdate(false);
	}
}

void VGA_SetBlinking(const uint8_t enabled)
{
	LOG(LOG_VGA, LOG_NORMAL)("Blinking %u", enabled);
	if (enabled) {
		vga.draw.blinking = 1; // used to -1 but blinking is unsigned
		vga.attr.mode_control|=0x08;
		vga.tandy.mode_control|=0x20;
	} else {
		vga.draw.blinking = 0;
		vga.attr.mode_control&=~0x08;
		vga.tandy.mode_control&=~0x20;
	}
	const uint8_t b = (enabled ? 0 : 8);
	for (uint8_t i = 0; i < 8; ++i)
		TXT_BG_Table[i + 8] = (b + i) | ((b + i) << 8) |
		                      ((b + i) << 16) | ((b + i) << 24);
}

#ifdef VGA_KEEP_CHANGES
static void inline VGA_ChangesStart( void ) {
	vga.changes.start = vga.draw.address >> VGA_CHANGE_SHIFT;
	vga.changes.last = vga.changes.start;
	if ( vga.changes.lastAddress != vga.draw.address ) {
//		LOG_MSG("Address");
		VGA_DrawLine = VGA_Draw_Linear_Line;
		vga.changes.lastAddress = vga.draw.address;
	} else if ( render.fullFrame ) {
//		LOG_MSG("Full Frame");
		VGA_DrawLine = VGA_Draw_Linear_Line;
	} else {
//		LOG_MSG("Changes");
		VGA_DrawLine = VGA_Draw_Changes_Line;
	}
	vga.changes.active = true;
	vga.changes.checkMask = vga.changes.writeMask;
	vga.changes.clearMask = ~( 0x01010101 << (vga.changes.frame & 7));
	++vga.changes.frame;
	vga.changes.writeMask = 1 << (vga.changes.frame & 7);
}
#endif

static void VGA_VertInterrupt(uint32_t /*val*/)
{
	if ((!vga.draw.vret_triggered) &&
	    ((vga.crtc.vertical_retrace_end & 0x30) == 0x10)) {
		vga.draw.vret_triggered=true;
		if (GCC_UNLIKELY(machine==MCH_EGA)) PIC_ActivateIRQ(9);
	}
}

static void VGA_Other_VertInterrupt(uint32_t val)
{
	if (val)
		PIC_ActivateIRQ(5);
	else PIC_DeActivateIRQ(5);
}

static void VGA_DisplayStartLatch(uint32_t /*val*/)
{
	vga.config.real_start = vga.config.display_start & (vga.vmemwrap - 1);
	vga.draw.bytes_skip = vga.config.bytes_skip;
}

static void VGA_PanningLatch(uint32_t /*val*/)
{
	vga.draw.panning = vga.config.pel_panning;
}

static void VGA_VerticalTimer(uint32_t /*val*/)
{
	vga.draw.delay.framestart = PIC_FullIndex();
	PIC_AddEvent(VGA_VerticalTimer, vga.draw.delay.vtotal);

	switch(machine) {
	case MCH_PCJR:
	case MCH_TANDY:
		// PCJr: Vsync is directly connected to the IRQ controller
		// Some earlier Tandy models are said to have a vsync interrupt too
		PIC_AddEvent(VGA_Other_VertInterrupt, vga.draw.delay.vrstart, 1);
		PIC_AddEvent(VGA_Other_VertInterrupt, vga.draw.delay.vrend, 0);
		// fall-through
	case MCH_CGA:
	case MCH_HERC:
		// MC6845-powered graphics: Loading the display start latch happens somewhere
		// after vsync off and before first visible scanline, so probably here
		VGA_DisplayStartLatch(0);
		break;
	case MCH_VGA:
		PIC_AddEvent(VGA_DisplayStartLatch, vga.draw.delay.vrstart);
		PIC_AddEvent(VGA_PanningLatch, vga.draw.delay.vrend);
		// EGA: 82c435 datasheet: interrupt happens at display end
		// VGA: checked with scope; however disabled by default by
		// jumper on VGA boards add a little amount of time to make sure
		// the last drawpart has already fired
		PIC_AddEvent(VGA_VertInterrupt, vga.draw.delay.vdend + 0.005);
		break;
	case MCH_EGA:
		PIC_AddEvent(VGA_DisplayStartLatch, vga.draw.delay.vrend);
		PIC_AddEvent(VGA_VertInterrupt, vga.draw.delay.vdend + 0.005);
		break;
	default:
		E_Exit("This new machine needs implementation in VGA_VerticalTimer too.");
		break;
	}

	//Check if we can actually render, else skip the rest (frameskip)
	++vga.draw.cursor.count; // Do this here, else the cursor speed depends
	                         // on the frameskip
	if (vga.draw.vga_override || !RENDER_StartUpdate()) {
		return;
	}

	vga.draw.address_line = vga.config.hlines_skip;
	if (IS_EGAVGA_ARCH) {
		vga.draw.split_line = (vga.config.line_compare + 1) / vga.draw.lines_scaled;
		if ((svgaCard==SVGA_S3Trio) && (vga.config.line_compare==0)) vga.draw.split_line=0;
		vga.draw.split_line -= vga.draw.vblank_skip;
	} else {
		vga.draw.split_line = 0x10000;	// don't care
	}
	vga.draw.address = vga.config.real_start;
	vga.draw.byte_panning_shift = 0;
	// go figure...
	if (machine==MCH_EGA) {
		if (vga.draw.doubleheight) // Spacepigs EGA Megademo
			vga.draw.split_line*=2;
		++vga.draw.split_line; // EGA adds one buggy scanline
	}
//	if (machine==MCH_EGA) vga.draw.split_line = ((((vga.config.line_compare&0x5ff)+1)*2-1)/vga.draw.lines_scaled);
#ifdef VGA_KEEP_CHANGES
	bool startaddr_changed=false;
#endif
	switch (vga.mode) {
	case M_EGA:
		if (!(vga.crtc.mode_control&0x1)) vga.draw.linear_mask &= ~0x10000;
		else vga.draw.linear_mask |= 0x10000;
		[[fallthrough]];
	case M_LIN4:
		vga.draw.byte_panning_shift = 8;
		vga.draw.address += vga.draw.bytes_skip;
		vga.draw.address *= vga.draw.byte_panning_shift;
		if (machine!=MCH_EGA) vga.draw.address += vga.draw.panning;
#ifdef VGA_KEEP_CHANGES
		startaddr_changed=true;
#endif
		break;
	case M_VGA:
		if (vga.config.compatible_chain4 && (vga.crtc.underline_location & 0x40)) {
			vga.draw.linear_base = vga.fastmem;
			vga.draw.linear_mask = 0xffff;
		} else {
			vga.draw.linear_base = vga.mem.linear;
			vga.draw.linear_mask = vga.vmemwrap - 1;
		}
		[[fallthrough]];
	case M_LIN8:
	case M_LIN15:
	case M_LIN24:
	case M_LIN16:
	case M_LIN32:
		vga.draw.byte_panning_shift = 4;
		vga.draw.address += vga.draw.bytes_skip;
		vga.draw.address *= vga.draw.byte_panning_shift;
		vga.draw.address += vga.draw.panning;
#ifdef VGA_KEEP_CHANGES
		startaddr_changed=true;
#endif
		break;
	case M_TEXT:
		vga.draw.byte_panning_shift = 2;
		vga.draw.address += vga.draw.bytes_skip;
		// fall-through
	case M_TANDY_TEXT:
	case M_CGA_TEXT_COMPOSITE:
	case M_HERC_TEXT:
		if (machine==MCH_HERC) vga.draw.linear_mask = 0xfff; // 1 page
		else if (IS_EGAVGA_ARCH) vga.draw.linear_mask = 0x7fff; // 8 pages
		else vga.draw.linear_mask = 0x3fff; // CGA, Tandy 4 pages
		vga.draw.cursor.address=vga.config.cursor_start*2;
		vga.draw.address *= 2;

		/* check for blinking and blinking change delay */
		FontMask[1]=(vga.draw.blinking & (vga.draw.cursor.count >> 4)) ?
			0 : 0xffffffff;
		/* if blinking is enabled, 'blink' will toggle between true
		 * and false. Otherwise it's true */
		vga.draw.blink = ((vga.draw.blinking & (vga.draw.cursor.count >> 4))
			|| !vga.draw.blinking) ? true:false;
		break;
	case M_HERC_GFX:
	case M_CGA2:
	case M_CGA4: vga.draw.address = (vga.draw.address * 2) & 0x1fff; break;
	case M_CGA16:
	case M_CGA2_COMPOSITE:
	case M_CGA4_COMPOSITE:
	case M_TANDY2:
	case M_TANDY4:
	case M_TANDY16: vga.draw.address *= 2; break;
	default:
		break;
	}
	if (GCC_UNLIKELY(vga.draw.split_line==0)) VGA_ProcessSplit();
#ifdef VGA_KEEP_CHANGES
	if (startaddr_changed) VGA_ChangesStart();
#endif

	// check if some lines at the top off the screen are blanked
	double draw_skip = 0.0;
	if (GCC_UNLIKELY(vga.draw.vblank_skip)) {
		draw_skip = vga.draw.delay.htotal * static_cast<double>(vga.draw.vblank_skip);
		vga.draw.address += vga.draw.address_add * vga.draw.vblank_skip / vga.draw.address_line_total;
	}

	// add the draw event
	switch (vga.draw.mode) {
	case PART:
		if (GCC_UNLIKELY(vga.draw.parts_left)) {
			LOG(LOG_VGAMISC, LOG_NORMAL)("Parts left: %u", vga.draw.parts_left);
			PIC_RemoveEvents(VGA_DrawPart);
			RENDER_EndUpdate(true);
		}
		vga.draw.lines_done = 0;
		vga.draw.parts_left = vga.draw.parts_total;
		PIC_AddEvent(VGA_DrawPart, vga.draw.delay.parts + draw_skip, vga.draw.parts_lines);
		break;
	case DRAWLINE:
	case EGALINE:
		if (GCC_UNLIKELY(vga.draw.lines_done < vga.draw.lines_total)) {
			LOG(LOG_VGAMISC, LOG_NORMAL)("Lines left: %d",
			                             static_cast<int>(vga.draw.lines_total - vga.draw.lines_done));
			if (vga.draw.mode == EGALINE)
				PIC_RemoveEvents(VGA_DrawEGASingleLine);
			else
				PIC_RemoveEvents(VGA_DrawSingleLine);
			RENDER_EndUpdate(true);
		}
		vga.draw.lines_done = 0;
		if (vga.draw.mode==EGALINE)
			PIC_AddEvent(VGA_DrawEGASingleLine,
			             vga.draw.delay.per_line_ms + draw_skip);
		else
			PIC_AddEvent(VGA_DrawSingleLine,
			             vga.draw.delay.per_line_ms + draw_skip);
		break;
	}
}

void VGA_CheckScanLength(void) {
	switch (vga.mode) {
	case M_EGA:
	case M_LIN4:
		vga.draw.address_add=vga.config.scan_len*16;
		break;
	case M_VGA:
	case M_LIN8:
	case M_LIN15:
	case M_LIN16:
	case M_LIN24:
	case M_LIN32:
		vga.draw.address_add=vga.config.scan_len*8;
		break;
	case M_TEXT:
		vga.draw.address_add=vga.config.scan_len*4;
		break;
	case M_CGA2:
	case M_CGA4:
	case M_CGA16: vga.draw.address_add = 80; break;
	case M_TANDY2:
		if (machine == MCH_PCJR) {
			vga.draw.address_add = vga.draw.blocks / 4;
			break;
		}
		[[fallthrough]];
	case M_CGA2_COMPOSITE: vga.draw.address_add = vga.draw.blocks; break;
	case M_TANDY4:
	case M_CGA4_COMPOSITE: vga.draw.address_add = vga.draw.blocks; break;
	case M_TANDY16:
		vga.draw.address_add=vga.draw.blocks;
		break;
	case M_TANDY_TEXT:
	case M_CGA_TEXT_COMPOSITE:
	case M_HERC_TEXT:
		vga.draw.address_add=vga.draw.blocks*2;
		break;
	case M_HERC_GFX:
		vga.draw.address_add=vga.draw.blocks;
		break;
	default:
		vga.draw.address_add=vga.draw.blocks*8;
		break;
	}
}

// If the hardware mouse cursor is activated, this function changes the VGA line
// drawing function-pointers to call the more complicated hardware cusror
// routines (for the given color depth).

// If the hardware cursor isn't activated, the simply fallback to the normal
// line-drawing routines for a given bit-depth.

// Finally, return the current mode's bits per line buffer value.
uint8_t VGA_ActivateHardwareCursor()
{
	uint8_t bit_per_line_pixel = 0;

	const bool use_hw_cursor = (svga.hardware_cursor_active &&
	                            svga.hardware_cursor_active());

	switch (vga.mode) {
	case M_LIN32: // 32-bit true-colour VESA
		bit_per_line_pixel = 32;

		VGA_DrawLine = use_hw_cursor ? VGA_Draw_LIN32_Line_HWMouse
		                             : VGA_Draw_Linear_Line;
		//
		// Use the "VGA_Draw_Linear_Line" routine that skips the DAC
		// 8-bit palette LUT and prepares the true-colour pixels for
		// rendering.
		break;
	case M_LIN24: // 24-bit true-colour VESA
		bit_per_line_pixel = 24;

		VGA_DrawLine = use_hw_cursor ? VGA_Draw_LIN32_Line_HWMouse
		                             : VGA_Draw_Linear_Line;
		break;
	case M_LIN16: // 16-bit high-colour VESA
		bit_per_line_pixel = 16;

		VGA_DrawLine = use_hw_cursor ? VGA_Draw_LIN16_Line_HWMouse
		                             : VGA_Draw_Linear_Line;
		break;
	case M_LIN15: // 15-bit high-colour VESA
		bit_per_line_pixel = 15;

		VGA_DrawLine = use_hw_cursor ? VGA_Draw_LIN16_Line_HWMouse
		                             : VGA_Draw_Linear_Line;
		break;
	case M_LIN8: // 8-bit and below
	default:
		bit_per_line_pixel = 32;

		// Use routines that treats the 8-bit pixel values as
		// indexes into the DAC's palette LUT. The palette LUT
		// is populated with 32-bit RGB's (XRGB888) pre-scaled
		// from 18-bit RGB666 values that have been written by
		// the user-space software via DAC IO write registers.
		//
		VGA_DrawLine = use_hw_cursor
		                     ? draw_unwrapped_line_from_dac_palette_with_hwcursor
		                     : draw_unwrapped_line_from_dac_palette;
		break;
	}
	assert(bit_per_line_pixel != 0);
	return bit_per_line_pixel;
}

static bool is_width_low_resolution(const uint16_t width)
{
	return width <= 320 || vga.seq.clocking_mode.is_pixel_doubling;
}

// A single point to set total drawn lines and update affected delay values
static void setup_line_drawing_delays(const uint32_t total_lines)
{
	vga.draw.parts_total = total_lines > 480 ? 1 : total_lines;

	vga.draw.delay.parts = vga.draw.delay.vdend / vga.draw.parts_total;

	assert(total_lines > 0 && total_lines <= SCALER_MAXHEIGHT);
	vga.draw.lines_total = total_lines;

	assert(vga.draw.parts_total > 0);
	vga.draw.parts_lines = total_lines / vga.draw.parts_total;

	assert(vga.draw.delay.vdend > 0.0);
	vga.draw.delay.per_line_ms = vga.draw.delay.vdend / total_lines;
}

// Determines pixel size as a pair of fractions (width and height)
static std::pair<Fraction, Fraction> determine_pixel_size(const uint32_t htotal,
                                                          const uint32_t vtotal)
{
	// bit 6 - Horizontal Sync Polarity. Negative if set
	// bit 7 - Vertical Sync Polarity. Negative if set
	//
	// Bits 6-7 indicate the number of displayed lines:
	//   1: 400, 2: 350, 3: 480
	auto horiz_sync_polarity = vga.misc_output >> 6;

	// TODO We do default to 9-pixel characters in standard VGA text modes,
	// meaning the below original assumptions might be incorrect for VGA
	// text modes:
	//
	// Base pixel width around 100 clocks horizontal
	// For 9 pixel text modes this should be changed, but we don't support
	// that anyway :) Seems regular vga only listens to the 9 char pixel
	// mode with character mode enabled
	const Fraction pwidth = {100, htotal};

	// Base pixel height around vertical totals of modes that have 100
	// clocks horizontally. Different sync values gives different scaling of
	// the whole vertical range; VGA monitors just seems to thighten or
	// widen the whole vertical range.
	Fraction pheight      = {};
	uint16_t target_total = 449;

	switch (horiz_sync_polarity) {
	case 0: // 340-line mode, filled with 449 total lines
		// This is not defined in vga specs,
		// Kiet, seems to be slightly less than 350 on my monitor

		pheight = Fraction(480, 340) * Fraction(target_total, vtotal);
		break;

	case 1: // 400-line mode, filled with 449 total lines
		pheight = Fraction(480, 400) * Fraction(target_total, vtotal);
		break;

	case 2: // 350-line mode, filled with 449 total lines
		// This mode seems to get regular 640x400 timings and goes for a
		// long retrace. Depends on the monitor to stretch the image.
		pheight = Fraction(480, 350) * Fraction(target_total, vtotal);
		break;

	case 3: // 480-line mode, filled with 525 total lines
	default:
		// TODO This seems like an arbitrary hack and should probably be
		// removed in the future. But it should make much of a difference in
		// the grand scheme of things, so leaving it alone for now.
		//
		// Allow 527 total lines ModeX modes to have exact 1:1 aspect
		// ratio.
		target_total = (vga.mode == M_VGA && vtotal == 527) ? 527 : 525;
		pheight = Fraction(480, 480) * Fraction(target_total, vtotal);
		break;
	}

	return {pwidth, pheight};
}

struct DisplayTimings {
	uint32_t total          = 0;
	uint32_t display_end    = 0;
	uint32_t blanking_start = 0;
	uint32_t blanking_end   = 0;
	uint32_t retrace_start  = 0;
	uint32_t retrace_end    = 0;
};

struct VgaTimings {
	uint32_t clock        = 0;
	DisplayTimings horiz  = {};
	DisplayTimings vert   = {};
};

// This function reads various VGA registers to calculate the display timings,
// but does not modify any of them as a side-effect.
static VgaTimings calculate_vga_timings()
{
	uint32_t clock       = 0;
	DisplayTimings horiz = {};
	DisplayTimings vert  = {};

	if (IS_EGAVGA_ARCH) {
		horiz.total          = vga.crtc.horizontal_total;
		horiz.display_end    = vga.crtc.horizontal_display_end;
		horiz.blanking_end   = vga.crtc.end_horizontal_blanking & 0x1F;
		horiz.blanking_start = vga.crtc.start_horizontal_blanking;
		horiz.retrace_start  = vga.crtc.start_horizontal_retrace;

		vert.total = vga.crtc.vertical_total |
		             ((vga.crtc.overflow & 1) << 8);

		vert.display_end = vga.crtc.vertical_display_end |
		                   ((vga.crtc.overflow & 2) << 7);

		vert.blanking_start = vga.crtc.start_vertical_blanking |
		                      ((vga.crtc.overflow & 0x08) << 5);

		vert.retrace_start = vga.crtc.vertical_retrace_start +
		                     ((vga.crtc.overflow & 0x04) << 6);

		if (IS_VGA_ARCH) {
			// Additional bits only present on VGA cards
			horiz.total |= (vga.s3.ex_hor_overflow & 0x1) << 8;
			horiz.total += 3;
			horiz.display_end |= (vga.s3.ex_hor_overflow & 0x2) << 7;
			horiz.blanking_end |= (vga.crtc.end_horizontal_retrace & 0x80) >> 2;
			horiz.blanking_start |= (vga.s3.ex_hor_overflow & 0x4) << 6;
			horiz.retrace_start |= (vga.s3.ex_hor_overflow & 0x10) << 4;

			vert.total |= (vga.crtc.overflow & 0x20) << 4;
			vert.total |= (vga.s3.ex_ver_overflow & 0x1) << 10;
			vert.display_end |= (vga.crtc.overflow & 0x40) << 3;
			vert.display_end |= (vga.s3.ex_ver_overflow & 0x2) << 9;
			vert.blanking_start |= vga.crtc.maximum_scan_line.start_vertical_blanking_bit9 << 4;
			vert.blanking_start |= (vga.s3.ex_ver_overflow & 0x4) << 8;
			vert.retrace_start |= ((vga.crtc.overflow & 0x80) << 2);
			vert.retrace_start |= (vga.s3.ex_ver_overflow & 0x10) << 6;
			vert.blanking_end = vga.crtc.end_vertical_blanking & 0x7f;
		} else { // EGA
			vert.blanking_end = vga.crtc.end_vertical_blanking & 0x1f;
		}

		horiz.total += 2;
		vert.total += 2;
		horiz.display_end += 1;
		vert.display_end += 1;

		horiz.blanking_end = horiz.blanking_start +
		                     ((horiz.blanking_end - horiz.blanking_start) &
		                      0x3F);
		horiz.retrace_end = vga.crtc.end_horizontal_retrace & 0x1f;
		horiz.retrace_end = (horiz.retrace_end - horiz.retrace_start) & 0x1f;

		if (!horiz.retrace_end) {
			horiz.retrace_end = horiz.retrace_start + 0x1f + 1;
		} else {
			horiz.retrace_end = horiz.retrace_start + horiz.retrace_end;
		}

		vert.retrace_end = vga.crtc.vertical_retrace_end & 0xF;
		vert.retrace_end = (vert.retrace_end - vert.retrace_start) & 0xF;

		if (!vert.retrace_end) {
			vert.retrace_end = vert.retrace_start + 0xf + 1;
		} else {
			vert.retrace_end = vert.retrace_start + vert.retrace_end;
		}

		// Special case for vert.blanking_start == 0:
		// Most graphics cards agree that lines zero to vertical blanking end
		// are blanked.
		// Tsend ET4000 doesn't blank at all if vert.blanking_start == vert.blanking_end.
		// ET3000 blanks lines 1 to vert.blanking_end (255/6 lines).
		if (vert.blanking_start != 0) {
			vert.blanking_start += 1;
			vert.blanking_end = (vert.blanking_end - vert.blanking_start) & 0x7f;
			if (!vert.blanking_end) {
				vert.blanking_end = vert.blanking_start + 0x7f + 1;
			} else {
				vert.blanking_end = vert.blanking_start + vert.blanking_end;
			}
		}
		vert.blanking_end++;

		if (svga.get_clock) {
			clock = svga.get_clock();
		} else {
			switch ((vga.misc_output >> 2) & 3) {
			case 0:
				clock = (machine == MCH_EGA) ? 14318180 : 25175000;
				break;
			case 1:
			default:
				clock = (machine == MCH_EGA) ? 16257000 : 28322000;
				break;
			}
		}

		// Adjust the VGA clock frequency based on the Clocking Mode
		// Register's 9/8 Dot Mode. See Timing Model:
		// https://wiki.osdev.org/VGA_Hardware
		clock /= vga.seq.clocking_mode.is_eight_dot_mode
		               ? PixelsPerChar::Eight
		               : PixelsPerChar::Nine;

		// Adjust the horizontal frequency if in pixel-doubling mode
		// (clock/2)
		if (vga.seq.clocking_mode.is_pixel_doubling) {
			horiz.total *= 2;
		}

	} else {
		horiz.total          = vga.other.htotal + 1;
		horiz.display_end    = vga.other.hdend;
		horiz.blanking_start = horiz.display_end;
		horiz.blanking_end   = horiz.total;
		horiz.retrace_start  = vga.other.hsyncp;
		horiz.retrace_end    = horiz.retrace_start + vga.other.hsyncw;

		vert.total = vga.draw.address_line_total * (vga.other.vtotal + 1) +
		             vga.other.vadjust;

		vert.display_end = vga.draw.address_line_total * vga.other.vdend;
		vert.retrace_start = vga.draw.address_line_total * vga.other.vsyncp;
		vert.retrace_end = vert.retrace_start + 16; // vsync width is
		                                            // fixed to 16 lines
		                                            // on the MC6845
		                                            // TODO Tandy
		vert.blanking_start = vert.display_end;
		vert.blanking_end   = vert.total;

		switch (machine) {
		case MCH_CGA:
		case MCH_PCJR:
		case MCH_TANDY:
			clock = ((vga.tandy.mode_control & 1) ? 14318180
			                                      : (14318180 / 2)) /
			        8;
			break;
		case MCH_HERC:
			if (vga.herc.mode_control & 0x2) {
				clock = 16000000 / 16;
			} else {
				clock = 16000000 / 8;
			}
			break;
		default: clock = 14318180; break;
		}
	}

	LOG_MSG("VGA: h total %u end %u blank (%u/%u) retrace (%u/%u)",
	        horiz.total,
	        horiz.display_end,
	        horiz.blanking_start,
	        horiz.blanking_end,
	        horiz.retrace_start,
	        horiz.retrace_end);

	LOG_MSG("VGA: v total %u end %u blank (%u/%u) retrace (%u/%u)",
	        vert.total,
	        vert.display_end,
	        vert.blanking_start,
	        vert.blanking_end,
	        vert.retrace_start,
	        vert.retrace_end);

	return {clock, horiz, vert};
}

struct UpdatedTimings {
	uint32_t horiz_display_end = 0;
	uint32_t vert_display_end  = 0;
	uint32_t vblank_skip       = 0;
};

static UpdatedTimings update_vga_timings(const VgaTimings& timings)
{
	const auto vert  = timings.vert;
	const auto horiz = timings.horiz;

	const auto fps     = VGA_GetPreferredRate();
	const auto f_clock = fps * vert.total * horiz.total;

	// Horizontal total (that's how long a line takes with whistles and bells)
	vga.draw.delay.htotal = static_cast<double>(horiz.total) * 1000.0 /
	                        f_clock; //  milliseconds
									 //
	// Start and End of horizontal blanking
	vga.draw.delay.hblkstart = static_cast<double>(horiz.blanking_start) *
	                           1000.0 / f_clock; //  milliseconds
	vga.draw.delay.hblkend = static_cast<double>(horiz.blanking_end) *
	                         1000.0 / f_clock;

	// Start and End of horizontal retrace
	vga.draw.delay.hrstart = static_cast<double>(horiz.retrace_start) *
	                         1000.0 / f_clock;
	vga.draw.delay.hrend = static_cast<double>(horiz.retrace_end) * 1000.0 /
	                       f_clock;

	// Start and End of vertical blanking
	vga.draw.delay.vblkstart = static_cast<double>(vert.blanking_start) *
	                           vga.draw.delay.htotal;
	vga.draw.delay.vblkend = static_cast<double>(vert.blanking_end) *
	                         vga.draw.delay.htotal;

	// Start and End of vertical retrace pulse
	vga.draw.delay.vrstart = static_cast<double>(vert.retrace_start) *
	                         vga.draw.delay.htotal;
	vga.draw.delay.vrend = static_cast<double>(vert.retrace_end) *
	                       vga.draw.delay.htotal;

	// Vertical blanking tricks
	auto vert_display_end  = vert.display_end;
	auto horiz_display_end = horiz.display_end;

	uint32_t vblank_skip = 0;
	if (IS_VGA_ARCH) {
		// Others need more investigation
		if (vert.blanking_start < vert.total) {
			// There will be no blanking at all otherwise
			//
			if (vert.blanking_end > vert.total) {
				// blanking wraps to the start of the screen
				vblank_skip = vert.blanking_end & 0x7f;

				// On blanking wrap to 0, the first line is not blanked this
				// is used by the S3 BIOS and other S3 drivers in some SVGA
				// modes
				if ((vert.blanking_end & 0x7f) == 1) {
					vblank_skip = 0;
				}

				// It might also cut some lines off the bottom
				if (vert.blanking_start < vert.display_end) {
					vert_display_end = vert.blanking_start;
				}
				LOG(LOG_VGA, LOG_WARN)
				("Blanking wrap to line %u", vblank_skip);

			} else if (vert.blanking_start <= 1) {
				// Blanking is used to cut lines at the start of the screen
				vblank_skip = vert.blanking_end;
				LOG(LOG_VGA, LOG_WARN)
				("Upper %u lines of the screen blanked", vblank_skip);

			} else if (vert.blanking_start < vert.display_end) {
				if (vert.blanking_end < vert.display_end) {
					// The game wants a black bar somewhere on the screen
					LOG(LOG_VGA, LOG_WARN)
					("Unsupported blanking: line %u-%u",
					 vert.blanking_start,
					 vert.blanking_end);
				} else {
					// Blanking is used to cut off some
					// lines from the bottom
					vert_display_end = vert.blanking_start;
				}
			}
			vert_display_end -= vblank_skip;
		}
	}
	// Display end
	vga.draw.delay.vdend = static_cast<double>(vert_display_end) *
	                       vga.draw.delay.htotal;

	// Check to prevent useless black areas
	if (horiz.blanking_start < horiz.display_end) {
		horiz_display_end = horiz.blanking_start;
	}
	if ((!IS_VGA_ARCH) && (vert.blanking_start < vert_display_end)) {
		vert_display_end = vert.blanking_start;
	}

	return {horiz_display_end, vert_display_end, vblank_skip};
}

void VGA_SetupDrawing(uint32_t /*val*/)
{
	if (vga.mode == M_ERROR) {
		PIC_RemoveEvents(VGA_VerticalTimer);
		PIC_RemoveEvents(VGA_PanningLatch);
		PIC_RemoveEvents(VGA_DisplayStartLatch);
		return;
	}

	// Set the drawing mode
	switch (machine) {
	case MCH_CGA:
	case MCH_PCJR:
	case MCH_TANDY:
		vga.draw.mode = DRAWLINE;
		break;
	case MCH_EGA:
		// Paradise SVGA uses the same panning mechanism as EGA
		vga.draw.mode = EGALINE;
		break;
	case MCH_VGA:
	default:
		vga.draw.mode = PART;
		break;
	}

	if (IS_EGAVGA_ARCH) {
		vga.draw.address_line_total = vga.crtc.maximum_scan_line.maximum_scan_line +
		                              1;
	} else {
		vga.draw.address_line_total = vga.other.max_scanline + 1;
	}

	const auto vga_timings = calculate_vga_timings();

	auto fake_double_scan = false;
	if (IS_VGA_ARCH) {
		const auto is_scan_doubled = vga.crtc.maximum_scan_line.is_scan_doubling_enabled;

		if (VGA_IsDoubleScanningVgaModes()) {
			if (is_scan_doubled) {
				vga.draw.address_line_total *= 2;
			}
		} else {
			// VGA machine not in EGA or VGA modes
			fake_double_scan = is_scan_doubled;
		}
	}

	if (!IS_EGAVGA_ARCH) {
		// in milliseconds
		vga.draw.delay.hdend = static_cast<double>(
		                               vga_timings.horiz.display_end) *
		                       1000.0 /
		                       static_cast<double>(vga_timings.clock);
	}

	// The screen refresh frequency and clock settings, per the DOS-mode
	vga.draw.dos_refresh_hz = static_cast<double>(vga_timings.clock) /
	                          (vga_timings.vert.total * vga_timings.horiz.total);

	const auto updated_timings = update_vga_timings(vga_timings);

	// EGA frequency dependent monitor palette
	if (machine == MCH_EGA) {
		if (vga.misc_output & 1) {
			// EGA card is in color mode
			if ((1.0 / vga.draw.delay.htotal) > 19.0) {
				// 64 color EGA mode
				VGA_ATTR_SetEGAMonitorPalette(EGA);
			} else {
				// 16 color CGA mode compatibility
				VGA_ATTR_SetEGAMonitorPalette(CGA);
			}
		} else {
			// EGA card in monochrome mode
			// It is not meant to be autodetected that way, you either
			// have a monochrome or color monitor connected and
			// the EGA switches configured appropriately.
			// But this would only be a problem if a program sets
			// the adapter to monochrome mode and still expects color output.
			// Such a program should be shot to the moon...
			VGA_ATTR_SetEGAMonitorPalette(MONO);
		}
	}

	Fraction pixel_aspect_ratio = {1};

	if (machine == MCH_EGA) {
		pixel_aspect_ratio = {CurMode->sheight * 4, CurMode->swidth * 3};
	} else {
		const auto [pwidth, pheight] = determine_pixel_size(
		        vga_timings.horiz.total, vga_timings.vert.total);
		pixel_aspect_ratio = pwidth / pheight;
	}

	vga.draw.resizing       = false;
	vga.draw.vret_triggered = false;

	auto width        = updated_timings.horiz_display_end;
	auto height       = updated_timings.vert_display_end;
	bool doubleheight = false;
	bool doublewidth  = false;

	unsigned bpp;
	switch (vga.mode) {
	case M_LIN15:
		bpp = 15;
		break;
	case M_LIN16:
		bpp = 16;
		break;
	case M_LIN24:
		bpp = 24;
		break;
	case M_LIN32:
	case M_CGA2_COMPOSITE:
	case M_CGA4_COMPOSITE:
	case M_CGA_TEXT_COMPOSITE: bpp = 32; break;
	default:
		bpp = 8;
		break;
	}

	vga.draw.linear_base = vga.mem.linear;
	vga.draw.linear_mask = vga.vmemwrap - 1;

	switch (vga.mode) {
	case M_VGA:
		width <<= 2;

		// Default to standard VGA linear drawing, which might be
		// upgraded to 18-bit palette drawing below.
		VGA_DrawLine = VGA_Draw_Linear_Line;

		// Consider double or single-scanning if the mode itself is
		// sub-350 line or the set width is assessed as a low resolution.
		if (CurMode->sheight < 350 || is_width_low_resolution(width)) {
			// Always round up odd number of lines
			const auto height_is_odd = (height % 2 != 0);
			if (height_is_odd) {
				height += 1;
			}

			const auto is_single_scan_permitted =
			        (vga.draw.vga_double_scan_handling !=
			         VgaDoubleScanHandling::DoubleScan);

			const auto is_line_freq_double_scanned =
			        (vga.draw.address_line_total == 2);

			// If requested, convert the double-scanned line to
			// single-scan
			if (is_single_scan_permitted && is_line_freq_double_scanned) {
				height /= 2;
				vga.draw.address_line_total /= 2;
				doubleheight = true;
			}
			// Width is always doubled for sub-350 line modes,
			// however if the user doesn't want aspect correction
			// (ie: "stretched to CRT dimensions") then let them see
			// art at 1:1 for the single-scanned frequencies (Eg:
			// 360x480 in Scorched Earth, and others).
			doublewidth = render.aspect ? true
			                            : is_line_freq_double_scanned;

			// The ReelMagic video mixer expects linear VGA drawing
			// (i.e.: Return to Zork's house intro), so limit the use
			// of 18-bit palettized LUT routine to non-mixed output.
			if (!ReelMagic_IsVideoMixerEnabled()) {
				bpp = 32;
				VGA_DrawLine = draw_linear_line_from_dac_palette;
			}
		}
		break;

	case M_LIN8:
		if (vga.crtc.mode_control & 0x8) {
			width >>= 1;
		} else if (svgaCard == SVGA_S3Trio && !(vga.s3.reg_3a & 0x10)) {
			doublewidth = true;
			width >>= 1;
		}
		[[fallthrough]];
	case M_LIN24:
	case M_LIN32:
		width <<= 3;
		if (vga.crtc.mode_control & 0x8) {
			doublewidth = true;
			if (vga.mode == M_LIN32 && CurMode->mode == 0x10f) {
				pixel_aspect_ratio /= 2;
			}
		}
		// Use HW mouse cursor drawer if enabled
		bpp = VGA_ActivateHardwareCursor();
		break;

	case M_LIN15:
	case M_LIN16:
		// 15/16 bpp modes double the horizontal values
		width <<= 2;
		if ((vga.crtc.mode_control & 0x8)) {
			doublewidth = true;
		} else {
			pixel_aspect_ratio *= 2;
		}
		// Use HW mouse cursor drawer if enabled
		bpp = VGA_ActivateHardwareCursor();
		break;

	case M_LIN4:
		doublewidth     = vga.seq.clocking_mode.is_pixel_doubling;
		vga.draw.blocks = width;
		width <<= 3;

		VGA_DrawLine = VGA_Draw_Linear_Line;

		vga.draw.linear_base = vga.fastmem;
		vga.draw.linear_mask = (static_cast<uint64_t>(vga.vmemwrap) << 1) - 1;
		break;

	case M_EGA:
		// Common settings for EGA and (S)VGA machine types
		vga.draw.blocks = width;
		width <<= 3;
		vga.draw.linear_base = vga.fastmem;
		vga.draw.linear_mask = (static_cast<uint64_t>(vga.vmemwrap) << 1) - 1;

		if (!IS_VGA_ARCH) {
			VGA_DrawLine = VGA_Draw_Linear_Line;
			break;
		}
		assert(IS_VGA_ARCH);

		VGA_DrawLine = draw_linear_line_from_dac_palette;

		bpp = 32;

		// Only EGA modes that are line-doubled need additional handling
		if (bit::is(CurMode->special, EGA_LINE_DOUBLE)) {
			const auto wants_single_scanning =
			        (vga.draw.vga_double_scan_handling !=
			         VgaDoubleScanHandling::DoubleScan);

			constexpr uint16_t ega_320x200 = 0xD;
			constexpr uint16_t ega_640x200 = 0xE;
			switch (CurMode->mode) {
			case ega_320x200:
				// Are we dealing with standard (matching)
				// dimensions?
				if (width == CurMode->swidth &&
				    height == CurMode->sheight * 2) {
					doublewidth  = true;
					doubleheight = wants_single_scanning;
				} else {
					// We're dealing with non-standard
					// dimensions
					doublewidth = !wants_single_scanning;

					// Adjust the aspect based on the custom
					// dimensions
					const auto scan_line_divisor = wants_single_scanning
					                                     ? 2
					                                     : 1;
					const auto rendered_mode_height =
					        render.aspect
					                ? 480
					                : (CurMode->sheight * 2);

					const Fraction w_scaler = {width,
					                           CurMode->swidth};

					const Fraction h_scaler = {rendered_mode_height,
					                           height};

					pixel_aspect_ratio *= (w_scaler * h_scaler /
					                       scan_line_divisor)
					                              .Inverse();
				}
				break;

			case ega_640x200:
				doublewidth  = wants_single_scanning;
				doubleheight = wants_single_scanning;
				pixel_aspect_ratio /= wants_single_scanning ? 2 : 1;
				break;

			default: break;
			}
		}
		break;

	case M_CGA16:
		pixel_aspect_ratio = {5, 6};
		doubleheight       = true;
		vga.draw.blocks    = width * 2;
		width <<= 4;

		VGA_DrawLine = VGA_Draw_CGA16_Line;
		break;

	case M_CGA2_COMPOSITE:
		pixel_aspect_ratio = {5, 6};
		doubleheight       = true;
		vga.draw.blocks    = width * 2;
		width <<= 4;

		VGA_DrawLine = VGA_Draw_CGA2_Composite_Line;
		break;

	case M_CGA4_COMPOSITE:
		vga.draw.blocks = width * 2;
		width <<= 4;
		pixel_aspect_ratio = {height * 4 * (doubleheight ? 2 : 1), width * 3};
		VGA_DrawLine = VGA_Draw_CGA4_Composite_Line;
		break;

	case M_CGA4:
		if (IS_VGA_ARCH) {
			if (vga.draw.vga_double_scan_handling ==
			    VgaDoubleScanHandling::DoubleScan) {
				pixel_aspect_ratio *= 2;
				// TODO (CGA4_DOUBLE_SCAN_WORKAROUND):
				//   Despite correctly line-doubling CGA
				//   200-line modes when VGA machines are
				//   double-scanning, we are currently unable to
				//   width-double them up to 640 columns at the
				//   VGA-draw level, so this aspect change is
				//   simply a work around. When this is
				//   fixed, please adjust the corresponding log
				//   work-around in sdlmain.cpp.
			} else {
				doublewidth = true;
			}
		}
		vga.draw.blocks = width * 2;
		width <<= 3;

		VGA_DrawLine = VGA_Draw_2BPP_Line;
		break;

	case M_CGA2:
		if (IS_VGA_ARCH && vga.draw.vga_double_scan_handling !=
		                           VgaDoubleScanHandling::DoubleScan) {
			doubleheight = true;
			doublewidth  = true;
			pixel_aspect_ratio /= 2;
		}
		vga.draw.blocks = 2 * width;
		width <<= 3;

		VGA_DrawLine = VGA_Draw_1BPP_Line;
		break;

	case M_TEXT:
		if (IS_VGA_ARCH) {
			bpp = 32;

			VGA_DrawLine = draw_text_line_from_dac_palette;

			vga.draw.pixels_per_character = vga.seq.clocking_mode.is_eight_dot_mode
			                                      ? PixelsPerChar::Eight
			                                      : PixelsPerChar::Nine;
		} else {
			vga.draw.pixels_per_character = PixelsPerChar::Eight;

			VGA_DrawLine = VGA_TEXT_Draw_Line;
		}
		vga.draw.blocks = width;
		doublewidth     = vga.seq.clocking_mode.is_pixel_doubling;
		width *= vga.draw.pixels_per_character;

		pixel_aspect_ratio *= {PixelsPerChar::Eight,
		                       vga.draw.pixels_per_character};
		break;

	case M_HERC_GFX:
		vga.draw.blocks = width * 2;
		width *= 16;

		assert(width > 0);
		assert(height > 0);
		pixel_aspect_ratio = {height * 4, width * 3};

		VGA_DrawLine = VGA_Draw_1BPP_Line;
		break;

		// clang-format off

		// Mode Control Registers 1 and 2 bit values for Tandy and PCJr (if different)
		// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// | Colors | Res     | double | MC1: 4 | 3   | 2+bw | 1+gfx | 0+Hi | MC2: 4 | 3     | 2   | 1   | 0 |
		// |--------|---------|--------|--------|-----|------|-------|------|--------|-------|-----|-----|---|
		// | 2      | 640x200 | yes    | 1 (0)  | -   | 0    | 1     | 0    | 0 (-)  | 0 (1) | -   | -   | - |
		// | 4-gray | 320x200 | no     | 0      | -   | 1    | 1     | 0    | 0 (-)  | 0     | -   | -   | - |
		// | 4      | 320x200 | no     | 0      | -   | 0    | 1     | 0    | 0 (-)  | 0     | -   | -   | - |
		// | 4      | 640x200 | yes    | 1 (0)  | -   | 0    | 1     | 1    | 0 (-)  | 1 (0) | -   | -   | - |
		// | 16     | 120x200 | no     | 0 (1)  | -   | 0    | 1     | 0    | 1 (-)  | 0     | -   | -   | - |
		// | 16     | 320x200 | no     | 0 (1)  | -   | 0    | 1     | 1    | 1 (-)  | 0     | -   | -   | - |

		// MC1 stands for the Mode control 1 register, bits four through zero.
		// MC2 stands for the Mode control 2 register, bits four through zero.
		//
		// Dosbox uses vga.tandy.mode_control and vga.tandy.gfx_control to
		// represent the state of these registers. They are used
		// interchangably for Tandy and PCJr modes.
		//
		// References:
		// -
		// http://www.thealmightyguru.com/Wiki/images/3/3b/Tandy_1000_-_Manual_-_Technical_Reference.pdf:
		// pg 58.
		// -
		// http://bitsavers.trailing-edge.com/pdf/ibm/pc/pc_jr/PCjr_Technical_Reference_Nov83.pdf:
		// pg 2-59 to 2-69.

		// clang-format on

	case M_TANDY2:
		pixel_aspect_ratio = {5, 12};

		VGA_DrawLine = VGA_Draw_1BPP_Line;

		if (machine == MCH_PCJR) {
			doublewidth     = (vga.tandy.gfx_control & 0x8) == 0;
			vga.draw.blocks = width * (doublewidth ? 4 : 8);
			width           = vga.draw.blocks * 2;
		} else {
			doublewidth     = (vga.tandy.mode_control & 0x10) == 0;
			vga.draw.blocks = width * (doublewidth ? 1 : 2);
			width           = vga.draw.blocks * 8;
		}
		break;

	case M_TANDY4:
		if (machine == MCH_TANDY) {
			doublewidth = (vga.tandy.mode_control & 0b10000) == 0b00000;
		} else {
			doublewidth = (vga.tandy.mode_control & 0b00001) == 0b00000;
		}

		vga.draw.blocks    = width * 2;
		width              = vga.draw.blocks * 4;
		pixel_aspect_ratio = {height * 4, width * 3};

		if ((machine == MCH_TANDY && (vga.tandy.gfx_control & 0b01000)) ||
		    (machine == MCH_PCJR && (vga.tandy.mode_control == 0b01011))) {
			VGA_DrawLine = VGA_Draw_2BPPHiRes_Line;
		} else {
			VGA_DrawLine = VGA_Draw_2BPP_Line;
		}
		break;

	case M_TANDY16:
		pixel_aspect_ratio = {5, 6};
		doubleheight       = true;
		vga.draw.blocks    = width * 2;

		if (vga.tandy.mode_control & 0x1) {
			if ((machine == MCH_TANDY) &&
			    (vga.tandy.mode_control & 0b10000)) {
				doublewidth = false;
				vga.draw.blocks *= 2;
				width = vga.draw.blocks * 2;
			} else {
				doublewidth = true;
				width       = vga.draw.blocks * 2;
			}
			VGA_DrawLine = VGA_Draw_4BPP_Line;
		} else {
			doublewidth = true;
			width       = vga.draw.blocks * 4;

			VGA_DrawLine = VGA_Draw_4BPP_Line_Double;
		}
		break;

	case M_TANDY_TEXT:
		pixel_aspect_ratio = {5, 6};
		doublewidth        = (vga.tandy.mode_control & 0x1) == 0;
		doubleheight       = true;
		vga.draw.blocks    = width;
		width <<= 3;

		VGA_DrawLine = VGA_TEXT_Draw_Line;
		break;

	case M_CGA_TEXT_COMPOSITE:
		pixel_aspect_ratio = {5, 6};
		doubleheight       = true;
		vga.draw.blocks    = width;
		width <<= (((vga.tandy.mode_control & 0x1) != 0) ? 3 : 4);

		VGA_DrawLine = VGA_CGA_TEXT_Composite_Draw_Line;
		break;

	case M_HERC_TEXT:
		pixel_aspect_ratio = {350, 480};
		vga.draw.blocks    = width;
		width <<= 3;

		VGA_DrawLine = VGA_TEXT_Herc_Draw_Line;
		break;

	default: LOG_WARNING("VGA: Unhandled VGA mode %02Xh", vga.mode); break;
	}

	VGA_CheckScanLength();

	auto vblank_skip = updated_timings.vblank_skip;
	if (fake_double_scan) {
		if (IS_VGA_ARCH) {
			vblank_skip /= 2;
			height /= 2;
		}
		doubleheight = true;
	}

	vga.draw.vblank_skip = vblank_skip;
	setup_line_drawing_delays(height);
	vga.draw.line_length = width * ((bpp + 1) / 8);
#ifdef VGA_KEEP_CHANGES
	vga.changes.active    = false;
	vga.changes.frame     = 0;
	vga.changes.writeMask = 1;
#endif
	// Use square pixels for all non-tweaked modes with 4:3 storage pixel
	// aspect ratio (e.g., 320x240, 400x300, 640x480, 1024x768, etc.)
	// The calculated PARs for these modes are not exactly 1:1, but they are
	// intended to be displayed with square pixels.
	if (width == CurMode->swidth && height == CurMode->sheight) {
		constexpr auto display_aspect_ratio = 4.0 / 3.0;
		const auto storage_aspect_ratio = static_cast<double>(width) / height;
		if (storage_aspect_ratio == display_aspect_ratio) {
			// LOG_MSG("VGA: Overriding PAR to 1:1 because SAR == DAR == 4/3");
			pixel_aspect_ratio = {1};
		}
	}
	// 1280x1024 is an outlier that needs special handling to stretch it to
	// 4:3 display aspect ratio in all SVGA/VESA modes
	if (CurMode->swidth == 1280 && CurMode->sheight == 1024) {
		pixel_aspect_ratio = Fraction(4, 3) * Fraction(1024, 1280);
	}

	// need to change the vertical timing?
	bool fps_changed = false;
	const auto fps   = VGA_GetPreferredRate();

	if (fabs(vga.draw.delay.vtotal - 1000.0 / fps) > 0.0001) {
		fps_changed = true;
		vga.draw.delay.vtotal = 1000.0 / fps;
		VGA_KillDrawing();
		PIC_RemoveEvents(VGA_Other_VertInterrupt);
		PIC_RemoveEvents(VGA_VerticalTimer);
		PIC_RemoveEvents(VGA_PanningLatch);
		PIC_RemoveEvents(VGA_DisplayStartLatch);
		VGA_VerticalTimer(0);
	}

#if C_DEBUG
	LOG(LOG_VGA, LOG_NORMAL)("h total %2.5f (%3.2fkHz) blank(%02.5f/%02.5f) retrace(%02.5f/%02.5f)",
	                         vga.draw.delay.htotal, (1.0 / vga.draw.delay.htotal), vga.draw.delay.hblkstart,
	                         vga.draw.delay.hblkend, vga.draw.delay.hrstart, vga.draw.delay.hrend);

	LOG(LOG_VGA, LOG_NORMAL)("v total %2.5f (%3.2fHz) blank(%02.5f/%02.5f) retrace(%02.5f/%02.5f)",
	                         vga.draw.delay.vtotal, (1000.0 / vga.draw.delay.vtotal), vga.draw.delay.vblkstart,
	                         vga.draw.delay.vblkend, vga.draw.delay.vrstart, vga.draw.delay.vrend);
#endif

	// need to resize the output window?
	if ((width != vga.draw.width) || (height != vga.draw.height) ||
	    (vga.draw.doublewidth != doublewidth) ||
	    (vga.draw.doubleheight != doubleheight) ||
	    (vga.draw.pixel_aspect_ratio != pixel_aspect_ratio) ||
	    (vga.draw.bpp != bpp) || fps_changed) {
		VGA_KillDrawing();

		if (width > SCALER_MAXWIDTH || height > SCALER_MAXHEIGHT) {
			LOG_ERR("VGA: The calculated video resolution %ux%u will be limited to the maximum of %ux%u",
			        width,
			        height,
			        SCALER_MAXWIDTH,
			        SCALER_MAXHEIGHT);
		}

		vga.draw.width  = std::min(width, static_cast<uint32_t>(SCALER_MAXWIDTH));
		vga.draw.height = std::min(height, static_cast<uint32_t>(SCALER_MAXHEIGHT));

		vga.draw.doublewidth        = doublewidth;
		vga.draw.doubleheight       = doubleheight;
		vga.draw.pixel_aspect_ratio = pixel_aspect_ratio;
		vga.draw.bpp                = bpp;

		if (doubleheight) {
			vga.draw.lines_scaled = 2;
		} else {
			vga.draw.lines_scaled = 1;
		}

#if C_DEBUG
		LOG(LOG_VGA, LOG_NORMAL)("VGA: Width %u, Height %u, fps %.3f", width, height, fps);
		LOG(LOG_VGA, LOG_NORMAL)("VGA: %s width, %s height aspect %.3f, fake_double_scan: %s",
		        doublewidth ? "double" : "normal",
		        doubleheight ? "double" : "normal",
		        pixel_aspect_ratio.Inverse().ToDouble(),
		        fake_double_scan ? "yes" : "no");
#endif

		if (!vga.draw.vga_override) {
			RENDER_SetSize(width,
			               height,
			               doublewidth,
			               doubleheight,
			               pixel_aspect_ratio,
			               bpp,
			               fps);
		}
	} else {
		// Always log mode changes at a minimum
		static auto previous_mode = CurMode;
		if (CurMode != previous_mode) {
			GFX_LogDisplayProperties();
			previous_mode = CurMode;
		}
	}
}

void VGA_KillDrawing(void) {
	PIC_RemoveEvents(VGA_DrawPart);
	PIC_RemoveEvents(VGA_DrawSingleLine);
	PIC_RemoveEvents(VGA_DrawEGASingleLine);
	vga.draw.parts_left = 0;
	vga.draw.lines_done = ~0;
	if (!vga.draw.vga_override) RENDER_EndUpdate(true);
}

void VGA_SetOverride(bool vga_override) {
	if (vga.draw.vga_override!=vga_override) {

		if (vga_override) {
			VGA_KillDrawing();
			vga.draw.vga_override=true;
		} else {
			vga.draw.vga_override=false;
			vga.draw.width=0; // change it so the output window gets updated
			VGA_SetupDrawing(0);
		}
	}
}
