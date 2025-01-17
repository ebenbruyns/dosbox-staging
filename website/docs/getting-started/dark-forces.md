# Star Wars: Dark Forces

We will continue our trend of featuring a disproportionate amount of LucasArts
games in our guide. The game we'll tackle next is the [Star Wars: Dark
Forces](https://en.wikipedia.org/wiki/Star_Wars:_Dark_Forces) demo from 1995,
an example of the first-person shooter (FPS) genre, featuring fast
software-rendered 3D graphics.


## Installing the game

As per our usual routine, we'll create a new folder in `DOS Games` called
`Dark Forces`, and the `drives/c` subfolder in it. The [game demo](https://archive.org/details/StarWarsDarkForces) comes in
three ZIP archives,
[dforces1.zip](https://archive.org/download/StarWarsDarkForces/dforces1.zip),
[dforces2.zip](https://archive.org/download/StarWarsDarkForces/dforces2.zip),
and [dforces3.zip](https://archive.org/download/StarWarsDarkForces/dforces3.zip);
we'll need to copy their contents to our virtual C drive. Make sure to
download the [manual](https://archive.org/details/dark-forces-manual) as well.

There's a `README.TXT` included with the demo, it's recommended to read at
least the installation and the keyboard shortcuts sections (use `more
README.TXT`). The instructions tell us exactly what we need to do (this is why
you should always check out the included text files):


    I) INSTRUCTIONS FOR INSTALLING AND RUNNING DARK FORCES DEMO:

    1. Create a directory for the demo on your hard drive.
       (ex. C:\DFDEMO)

    2. Extract the demo using PKUNZIP.EXE into the demo
       directory.  If you have downloaded one or both of
       the cutscene files, then unzip them as well.

    3. From the demo directory, type "demo" and  RETURN. This will
       bring you to the install menu. From the install menu, follow
       the on-screen instructions. Press "I" to install. This will
       take you through through the sound card setup program. When
       the install is finished it will bring you to the options
       menu. From there you type "1" to run Dark Forces. In the
       future, in order to get to the options menu, you should
       type "demo" from the your demo directory.

We actually don't need that `DFDEMO` folder, and we've already extracted the
contents of the three ZIP archives to our root directory on drive C, so just
follow the instructions in step 3. Choose the *Express Setup* option in the
sound configuration utility by pressing ++e++, and the rest should be
straightforward. The setup program will detect the Sound Blaster 16 correctly
and will even play some test sounds.

After the setup has been completed, start the game by running `demo` again
and then pressing the ++1++ key.

<figure markdown>
  ![Star Wars: Dark Forces -- Opening scene](https://archive.org/download/dosbox-staging-v0.82.0-dark-forces-ingame/dark-forces-ingame.png){ loading=lazy }

  <figcaption markdown>
  Star Wars: Dark Forces --- Opening scene
  </figcaption>
</figure>


## Autotype

Having to press the ++1++ key every time we start the game is not the end of
the world, but it's not great either. Luckily, there's a way to automate that;
the handy `autotype` DOSBox Staging command can simulate keypresses as implied
by its name. This is what we need to put into our `[autoexec]` section to
start the game without manual intervention:

```ini
[autoexec]
c:
autotype -w 0.5 1
demo
exit
```

The first `-w 0.5` argument of the `autotype` command specifies an initial 0.5
second wait time before the "auto-typing" begins. This is followed by the `1`
argument which will simulate pressing the ++1++ key.

That's it! It's a simple but very useful feature. Run `autotype /?` to see the
full list of available options and check out the list of [AUTOTYPE
candidates](https://github.com/dosbox-staging/dosbox-staging/wiki/AUTOTYPE-Candidates)
on our wiki for assorted real-life examples.


## Display refresh rate

Dark Forces uses the 320&times;200 / 256-colour VGA screen mode, just like the
majority of DOS games from the 90s. This mode uses a fixed 70 Hz screen
refresh rate, so if your emulated CPU is fast enough, you can get 70 FPS out
of the game.

But there's a problem: most modern non-variable refresh rate flat screens work
at fixed 60 Hz. Luckily, DOSBox Staging intelligently times and drops frames
if there's a mismatch between the refresh rate of the *emulated* graphics
card and the *actual* refresh rate of your physical monitor. In most cases,
this works fine out of the box without you needing to configure anything.

However, even though DOSBox Staging is trying to do its best in such
situations, refresh rates will always result in some visible unevenness of
motion. This is especially noticeable in games that feature smooth scrolling
(e.g., shoot'em up games, platformers, and pinball games), and in fast-paced
3D games, such as Dark Forces.

To achieve smoother motion, we have two options:

- Using a **variable-refresh rate (VRR) monitor** --- Nvidia G-Sync, AMD
  FreeSync, VESA AdaptiveSync, and Apple ProMotion certified displays all
  fall into this category. Any monitor that can continuously vary the
  refresh rate from about 59 to 71 Hz should do the job. DOSBox Staging
  automatically takes advantage of such displays by default.

- Using **specific fixed refresh rates** on non-VRR monitors --- Even if
  you're not a lucky owner of a VRR display, your monitor might support a
  fixed 70 Hz refresh rate if you create a custom screen mode for it. This
  is highly monitor dependent, but there's no harm in trying it out.


Note, however, that depending on the type of game, the 60/70 Hz mismatch might
not matter much or at all. For instance, most adventure, strategy, and
role-playing games rarely have fast-paced graphics; they generally only update
the screen in the low 5-20 FPS range. Running the monitor at 70 Hz results in
very little noticeable improvement with these games.

Another important consideration is that most pre-VGA games don't feature
smooth movement in their graphics, so the general unevenness of motion masks
the slight judder resulting from the 60/70 Hz mismatch.

For reference, these are the refresh rates of all emulated graphics cards:

| Graphics adapter      | Refresh rate                                                                     |
|-----------------------|----------------------------------------------------------------------------------|
| SVGA and VESA         | 70 Hz or higher --- 640&times;480 or higher extended and VESA modes              |
| VGA                   | 60 Hz --- 640&times;480 standard mode only<br>70 Hz --- all other standard modes |
| CGA, PCjr, Tandy, EGA | 60 Hz                                                                            |
| Hercules              | 50 Hz                                                                            |

!!! tip "Creating custom 60 and 70 Hz screen modes"

    "Custom resolution" is a bit of a misnomer because what we're after is
    creating a new screen mode that uses your monitor's native resolution, but
    with a custom *refresh rate*.

    You can create such custom resolutions using the Nvidia Control Panel or
    with the [Custom Resolution Utility
    (CRU)](https://www.monitortests.com/forum/Thread-Custom-Resolution-Utility-CRU)
    on Windows. You'll probably need to enable *CVT reduced blanking* (CVT-RB
    or CVT-RBv2) to go up to 70 Hz. For the best results, it's recommended to
    use the exact fractional **59.713 Hz** and **70.086 Hz** refresh rates for
    the nominal "60 Hz" and "70 Hz" DOS rates, respectively.

    The drawback of this approach is that you need to set the appropriate
    custom resolution before starting DOSBox Staging, and if a game switches
    between different refresh rates, well, you're out of luck---these games
    can only work 100% correctly either on a true VRR display or a real
    old-school CRT monitor. However, most games only use a single fixed
    refresh rate, which is usually the standard 70 Hz VGA rate, so this
    technique is still very practical.


## Vertical syncing

Dark Forces synchronises its screen updates to the refresh rate of the
emulated VGA card, which is always 70 Hz. This is hardcoded behaviour that
cannot be disabled in the game's configuration.

DOSBox Staging itself also uses vertical syncing by default to synchronise
updating the emulated image to the vertical refresh rate of your physical
monitor. Without DOSBox-level vsync, fast-paced games such as this one would
exhibit a lot of tearing. This is important to understand: to get zero
tearing, vertical syncing *must* be happening both in the game *and* at the
DOSBox level!

Conversely, DOSBox-level vsync only means that the *emulated* image is output
to your physical monitor without flickering and tearing; the emulated DOS game
*itself* might or might not use vertical syncing, and thus might or might not
produce tearing artifacts of its own (these artifacts are present when running
the game on real hardware too---there's nothing we can do about them).

You don't have control over the vertical syncing behaviour in most DOS games.
For example, Commander Keen 4--6, Doom, and many other FPS games have
hardcoded vsync with a fixed 35 FPS cap. These games were programmed with
fixed refresh rates in mind from the ground up; trying to disable the vsync or
altering the FPS cap would completely break them (but the good news is you
can't do that anyway).

To recap, the following variations are possible when it comes to vsync:

<div class="compact vsync" markdown>

| Game uses vsync? | DOSBox-level vsync enabled? | Result
|-----|-----|---------------------
| yes | yes | no tearing
| no  | yes | some tearing; also present on real hardware
| yes | no  | bad tearing (in fast-paced games)
| no  | no  | very bad double-tearing (in fast-paced games)

</div>

<style>
.vsync th:nth-child(3) {
  width: 58%;
}
</style>

As you can see, there is no point in disabling vsync in DOSBox itself, and
even with DOSBox vsync enabled you might still get tearing in some games, but
that's just how the games were coded. We can't "fix" that at the emulator
level; that would require making changes to the games' code. On the other
hand, that's the "authentic DOS experience" for you! :sunglasses:


## Setting the emulated CPU speed

Hardware-accelerated 3D graphics only became widespread in the second half of
the 90s after the end of the DOS era, so most DOS games only use software
rendering. Dark Forces is no exception, which means your emulated CPU needs to
be fast enough to reach the 70 FPS maximum the game supports.

The game prints out the familiar message of the DOS/4GW DOS extender at
startup, which means this is a
[protected mode game](../beneath-a-steel-sky/#real-and-protected-mode):

``` { . .dos-prompt }
DOS/4GW Protected Mode Run-Time  Version 1.95
Copyright (c) Rational Systems, Inc. 1990-1993
```

As we've learnt, protected mode games default to using all available CPU
power. While this will most likely help achieve a stable 70 FPS (if your
computer is powerful enough), running the CPU at maximum speed could starve
the audio emulation, leading to audible glitches, clicks, and pops.

To avoid such issues, it's much preferable to emulate a CPU that is
powerful enough to handle 70 FPS but not more. One way to come up with a good
cycles number is to start the game with a relatively low cycles setting, say
10&thinsp;000, then keep increasing it with the ++ctrl+f12++ shortcut
(++cmd+f12++ on the Mac) while playing to arrive at the lowest value that
results in adequate performance. As a reminder, you can decrease the current
cycles value too with the ++ctrl+f11++ (++cmd+f11++) shortcut.

So let's add the following to our config:

```ini
[cpu]
cycles = 10000
```

Start the game and hold the ++left++ or ++right++ key to keep turning around,
then increase the cycles value until the motion becomes smooth (again, unless
you're using a VRR monitor or a fixed 70 Hz refresh rate, it will never become
completely smooth). Somewhere around 50&thinsp;000 cycles seems to do the
trick, which roughly corresponds to a Pentium 90 based on our [table of
common
processors](../beneath-a-steel-sky/#finding-the-correct-speed-for-a-game).

So we'll go with that setting. You might need to increase this if later levels
are more demanding on the CPU, but this is a good starting point:

```ini
[cpu]
cycles = 50000
```

!!! warning "Running out of steam"

    As you keep increasing the fixed cycles value, the audio will start
    breaking up, and the motion will become very stuttery after a certain
    point. This means you're trying to emulate a faster CPU than your computer
    can handle. If you're on a less powerful machine, this unfortunately can
    happen before reaching the minimum cycles value necessary for a smooth and
    stable 70 FPS.

    You have only two options in this situation: live with a lower cycles
    setting that results in less than 70 FPS, or buy a more powerful computer
    with a faster processor that can handle higher cycles values.

    Also, make sure you're not running any other CPU-intensive programs at the
    same time in the background. Virus scanners, backup software, copying
    large files to USB storage, heavy network traffic, and even ordinary web
    browsers with many tabs open can chew up a significant part of your total
    CPU power. Certain popular chat programs can be very demanding on the CPU
    too, and, surprisingly, even innocent-looking stuff such as automatic
    wallpaper changers can have negative effects on the performance of DOSBox.

    As a general rule, it's best to close all other programs if you're
    encountering performance issues with DOSBox and try again. Just apply
    common sense and treat DOSBox as any other performance-intensive modern
    game.


## Setting up General MIDI sound

Our previous example game, Beneath A Steel Sky, supports the Roland MT-32 MIDI
sound module to provide more realistic-sounding music than what the Sound
Blaster's OPL synthesiser is capable of. Dark Forces, however, was composed
for a newer family of MIDI sound modules that support the so-called **General
MIDI standard**.

DOSBox Staging comes with an integrated General MIDI compliant synthesiser
called FluidSynth. Unlike the MT-32 emulation, FluidSynth does not attempt to
mimic any specific hardware device but a very generic MIDI sound module. This
sound module doesn't have any built-in sounds---you need to load so-called
**SoundFont files** (`.sf2` extension) into FluidSynth to get any sound out of
it; these contain the instrument definitions and the sound data.

You can read more about the history of MIDI in DOS gaming at the end of this
section, but the short practical summary is as follows:

- The Roland Sound Canvas SC-55 was the de-facto standard General MIDI sound
  module until the very end of the DOS years (around 1997).

- DOSBox Staging does not emulate the SC-55 directly, but there are SoundFonts
  you can load into FluidSynth that approximate the sound of the SC-55.

One extra detail is that Roland went a bit beyond the General MIDI standard in
their Sound Canvas series; these modules actually support the **GS standard**,
which is basically General MIDI plus some Roland-specific enhancements. Many
games use these extra features, and while General MIDI compatible SoundFonts
will work with them, for the best results it's recommended to use GS standard
compatible ones.

One such SoundFont is [GeneralUser GS](https://schristiancollins.com/generaluser.php);
download the latest version, then copy the SoundFont file with the `.sf2`
extension into its designated folder:

<div class="compact" markdown>

| <!-- --> | <!-- -->
|----------|----------
| **Windows**  | `C:\Users\%USERNAME%\AppData\Local\DOSBox\soundfonts\`
| **macOS**    | `~/Library/Preferences/DOSBox/soundfonts/`
| **Linux**    | `~/.config/dosbox/soundfonts`

</div>

The next step is to configure DOSBox Staging to use this SoundFont for General
MIDI playback. The name of the SoundFont might be slightly different if a new
version has been released since the writing of this guide; make sure you use
the correct filename for the `soundfont` setting.

```ini
[midi]
mididevice = fluidsynth

[fluidsynth]
soundfont = "GeneralUser GS v1.471.sf2"
```

The only thing left to do is to reconfigure the game to use General MIDI for
music:

- Comment out the `autotype` command in the `[autoexec]` section and restart
  DOSBox.

- Press ++3++ in the startup menu to reconfigure the sound settings, then
  ++c++ to do a custom setup.

- Enter the *Advanced Menu* (it's above *Quit* at the bottom).

- Select the *General MIDI* device for *Music*, and select the default `330`
  value for the *Port* setting.

- The *Digital Sound* settings don't need changing as our Sound Blaster 16 has
  been auto-detected correctly.

The final configuration should look like this:

<figure markdown>
  ![Star Wars: Dark Forces setup --- Advanced sound settings](https://archive.org/download/dosbox-staging-getting-started-guide-assets/dark-forces-midi.png){ loading=lazy width=80% }
</figure>

Press ++enter++ on the *Test Music* menu item and you should hear the start of
the Imperial March playing with much more realistic instruments!

!!! important "How to drive a manual"

    Note that if you try to use *Detect* in the *Music* section, the
    auto-detection will fail and the *Music* option will revert to *None*.
    This can happen with some setup utilities, therefore it's important to
    know that the port of the General MIDI device must be set to **330**,
    should the auto-detection fail. This is sometimes called the *MPU-401
    port*, *MPU port*, or just *MIDI port*.

    Similarly, most games can auto-detect the Sound Blaster settings, but
    sometimes you need to enter them manually. The default DOSBox settings are
    port (I/O address) **220**, IRQ (interrupt) **7**, and DMA channel **1**.


It's worth watching the whole intro at this point to appreciate how much more
realistic the General MIDI rendition of this orchestral piece sounds compared
to the OPL synth.

Of course, you can always try other GM or GS compatible SoundFonts
too---that's the beauty of General MIDI!
[FluidR3_GM_GS](https://archive.org/details/fluidr3-gm-gs) and
[Creative Labs 4M GM_4gmgsmt](https://archive.org/download/free-soundfonts-sf2-2019-04/Creative%20Labs%204M%20GM_4gmgsmt.sf2)
are good candidates as they sound fairly close to the SC-55 sound set. The
[list of recommended SoundFonts](https://github.com/dosbox-staging/dosbox-staging/wiki/MIDI#soundfonts)
on our wiki should also give you some further pointers on which SoundFonts
are worth trying and where to find them.

Note that while some SoundFonts, such as GeneralUser GS, give you overall good
results in most games, other SoundFonts might excel in a handful of titles and
sound terrible in others. To give you an idea, here's how the intro music
sounds on the original Roland SC-55 hardware versus a few different
SoundFonts. As mentioned, GeneralUser GS and Creative 4GMGSMT are the closest
in terms of overall balance and character, the rest are rather hit-and-miss.
My advice: just pick one that gives you good overall results and stick with
it.

Further comparison recordings can be found on [our wiki](https://github.com/dosbox-staging/dosbox-staging/wiki/MIDI#comparison-recordings).

<div class="compact" markdown>
| <!-- --> | <!-- -->
| -------- | --------
| **Roland SC-55 v1.21 (hardware)** | <audio controls src="https://archive.org/download/FluidSynth-MIDI-Comparison/dark-forces-intro-sc-55-v1.21.mp3"> Your browser does not support the <code>audio</code> element.</audio>
| GeneralUser GS    | <audio controls src="https://archive.org/download/FluidSynth-MIDI-Comparison/dark-forces-intro-generaluser-gs.mp3"> Your browser does not support the <code>audio</code> element.</audio>
| Creative 4GMGSMT  | <audio controls src="https://archive.org/download/FluidSynth-MIDI-Comparison/dark-forces-intro-creative-4gmgsmt.mp3"> Your browser does not support the <code>audio</code> element.</audio>
| FluidR3_GM_GS     | <audio controls src="https://archive.org/download/FluidSynth-MIDI-Comparison/dark-forces-intro-fluidr3-gm.mp3"> Your browser does not support the <code>audio</code> element.</audio>
| FatBoy            | <audio controls src="https://archive.org/download/FluidSynth-MIDI-Comparison/dark-forces-intro-fatboy.mp3"> Your browser does not support the <code>audio</code> element.</audio>
| Arachno           | <audio controls src="https://archive.org/download/FluidSynth-MIDI-Comparison/dark-forces-intro-arachno.mp3"> Your browser does not support the <code>audio</code> element.</audio>

</div>


!!! tip "Turn it down, please!"

    Some SoundFonts are *way* too loud; if you encounter such a SoundFont,
    you'll most likely want to lower the output level of FluidSynth. For
    example, to scale back the volume to 40%, use the `mixer fsynth 40`
    command. A few SoundFonts are _extremely loud_---you need to turn the
    FluidSynth volume down to 10-20% to get usable levels. It's a mystery what
    the authors were thinking...


!!! warning "Roland MT-32 is not General MIDI"

    Do not confuse the Roland MT-32 family of MIDI sound modules with General
    MIDI modules! Music composed for the MT-32 sounds *utterly wrong* on a
    General MIDI module, and vice versa. Yes, they both have "MIDI" in their
    names, but that only refers to the communication protocol. The MT-32 range
    of devices are fully programmable synthesisers and most games take
    advantage of their sound-shaping capabilities, while the General MIDI
    modules have much more modest synthesis capabilities, but feature more
    realistic-sounding real-world instruments (the soundtrack of Dark Forces
    showcases this very well---you just can't get such realistic orchestral
    music out of an MT-32).

    Whenever you want to configure a new game for MIDI sound, you should
    consult the [list of MT-32-compatible computer games](https://www.vogonswiki.com/index.php/List_of_MT-32-compatible_computer_games)
    to see if the game was originally composed for the MT-32 or a General MIDI
    device (typically the SC-55). The original game manuals very rarely give
    you this info, so you *absolutely* need this list that contains the
    hard-earned knowledge of many decades of research and testing.

    Quite confusingly, there's an alarmingly large list of games that falsely
    claim MT-32 compatibility in their setup utility, but they in fact require
    a General MIDI module to sound correct. Before you configure any game for
    MT-32 sound, you should definitely check the [list of games that falsely
    claim MT-32 compatibility](https://www.vogonswiki.com/index.php/List_of_MT-32-compatible_computer_games#Games_that_falsely_claim_MT-32_compatibility)
    too, no matter what the game's manual says (quite often, the information
    contained therein is simply wrong).


!!! info "A brief history of MIDI in DOS gaming"

    In the beginning, the iconic Roland MT-32 family of devices were the only
    MIDI sound modules available for DOS games that made realistic-sounding
    music a possibility. The original MT-32 was released in 1987, and its
    hegemony ended in 1991 with Roland unleashing its successor, the similarly
    iconic Roland Sound Canvas SC-55.

    The SC-55 was another pioneering effort that shaped DOS gaming history in
    significant ways: it was the world's first device to support the *General
    MIDI* standard and the *GS standard* (the latter being Roland's
    vendor-specific General MIDI extensions). MIDI in the broadest sense only
    defines a communication mechanism between MIDI-capable devices (e.g., a
    sound module and your computer), but remains silent on how the different
    instruments should *sound*. General MIDI (GM) remedied this with its 128
    standard instrument definitions; for example, you can be sure that on any
    GM-compliant module instrument \#1 is the Acoustic Grand Piano and
    instrument \#36 is the Fretless Bass.

    This made interoperation between GM-compliant devices possible. In theory,
    you could use any General MIDI sound module with any GM-compliant game,
    and you would get music, and it would sound *mostly* correct. In practice,
    that "mostly" part is highly variable and it depends on the musical
    composition and the particular sound module in use. The thing is, most
    composers and gamers had the SC-55, establishing it as a de-facto standard
    (understandably, because it was the very first GM module). Other GM sound
    modules can render musical compositions written for the SC-55 unbalanced
    and weird-sounding, depending on how different their instruments sound
    compared to the "standard" SC-55.

    Another thing that makes compatibility difficult to achieve is that the
    SC-55 supports not just the GM but also the GS standard, and many games
    make good use of certain GS-only features. This means that any other
    General MIDI module that claims "Sound Canvas compatibility" also has to
    implement the GS standard in a manner that sounds very close to the SC-55,
    which is not always the case.




## Dynamic range compression

If a game has loud music and you set the output level of your soundcard too
high, you'll get rather unpleasant-sounding distortion and crackling on real
hardware. DOSBox Staging has a dynamic range compressor on the master channel
that automatically pulls the volume back on loud sounds to prevent such
distortion from happening.

You can test this by playing the game for a minute with the GeneralUser GS
SoundFont. You might notice a slight "pumping" as the compressor pulls the
volume back on loud sound effects; this dips the volume level of the
background music as well.

To hear the distortion caused by the loud sounds clearly, disable the master
compressor:

```ini
[mixer]
compressor = off
```

Overall, the compressor does a good job of preventing audible distortion up to
a certain point, and most users should not think much about it; it just works.
But if the master channel is overdriven heavily, the pumping artifacts would
be rather distracting, so in that case, it's better to lower the master volume
with the `mixer` command (e.g., `mixer master 50` to set the master volume to
50%). People who are after the best audio quality should also prefer lowering
the master volume rather than relying on the compressor.

The sound output of Dark Forces is on the loud side, so we'll set the output
to 50% to make sure the compressor is never engaged:

```ini
[autoexec]
c:
mixer master 50
autotype -w 0.5 1
demo
exit
```

## Reverse stereo

The sound setup utility asks us to confirm whether the left and right audio
channels should be swapped when setting up digital audio. Weird as it may
sound, some Sound Blaster models and clones had "reversed stereo" for some
inexplicable reason. If people used any of these cards during the development
of a game, the stereo image would be reversed on normal cards, and vice versa.

Irrespective of the exact reasons, the fact is that some games with stereo
digital audio support have the left and right channels reversed. In affected
games, the channels might be reversed on all Sound Blaster variants, or only
on certain models.

In the case of Dark Forces, you don't need to use the "reverse stereo" option
when emulating a Sound Blaster 16 card (which is the default). However, when
using the Sound Blaster Pro 1 model (`sbtype = sbpro1`), the stereo image
needs to be reversed.

But what about games that don't have such a handy "reverse stereo" option in
their setup utility? For those games, you can simply reverse the left and
right channels of the `SB` mixer channel with the `mixer sb reverse` command.
(Remember, use `mixer /?` to list all available commands and options.)


## Minimising audio glitches

Even after setting the optimal cycles value for the game to limit the emulated
CPU speed, you might still hear occasional audio glitches. Why this can happen
depends on a lot of factors, but mainly it comes down to the particular
hardware, driver, and operating system combination you're using.

As a last resort, you can try setting larger audio buffers sizes; this usually
helps eliminate the occasional clicks and pops. The downside of this is that
it will also increase audio latency, meaning that you'll hear the audio in
response to some action with a longer delay (e.g., when shooting with a gun).

The default `blocksize` value is 1024 on Windows and 512 on macOS and Linux.
The default `prebuffer` size is 25 on Windows and 20 on the other platforms (this
is specified in milliseconds). You can experiment with increasing these
settings---doubling them is a good start. Generally, `prebuffer` can be
anything, and `blocksize` should be one of the following values: 256, 512,
1024, 2048, 4096, or 8192. 


```ini
[mixer]
blocksize = 2048
prebuffer = 50
```


## Final configuration

Here's the full config of Dark Forces set up for General MIDI sound:

```ini
[sdl]
fullscreen = on
viewport_resolution = 1120x840

[cpu]
# to emulate a Pentium 90
cycles = 50000

[midi]
mididevice = fluidsynth

[fluidsynth]
soundfont = "GeneralUser GS v1.471.sf2"

[autoexec]
c:
mixer master 50
autotype -w 0.5 1
demo
exit
```
