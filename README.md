hz - AAC/AACplus/AACplusV2 & MP1/MP2/MP3 Icecast/Shoutcast source client
========================================================================


What's the point?
=================
hz is a small, portable and fast MPEG1/2/2.5 LayerI/II/III and
AAC/AACplus/AACplusV2 Icecast/Shoutcast source client. It is written
to be nice and easy to use. It is also scriptable in some way, so
you can script your playlist handling with Lua scripting language.


What is supported?
==================
AAC/AACplus/AACplusV2 and MPEG1/MPEG2/MPEG2.5 LayerI/II/III files
can be streamed to a Icecast or Shoutcast server. All possible bitrates are
fully supported, even VBR, but gosh, VBR seems not to be a grand idea for
an internet radio, I suggest that you use CBR. But whatever...


Tell me more.
=============
Well, here are some key features:

 - AACx and MPx files support. All possible bitrates and their variations, including VBR.
 - Pretty precise timing. You shouldn't get any client buffering ever.
 - Icecast and Shoutcast servers are fully supported.
 - Metadata updating supported. The metadata is read from ID3v1 and ID3v2 tags.
   It can also be read from cuesheets (.cue file with the same name as audio file).
 - You can write your own script routine to get the next file name to stream.
 - You can write your own script routine to format metadata.


What about MP4, MP4A files support? And Ogg Vorbis?
===================================================
MP4 is a container, not a compression format. If your MP4 file
contains AAC compressed sound that you want to stream, you
have to extract it from MP4 first. You can do it with MPEG4IP
(http://www.mpeg4ip.net), for example.
Ogg Vorbis is not (yet) supported.


Is there to be a live source support, for example /dev/dsp?
===========================================================
Never. hz has been written to be small and fast, so you can run it even on
an old Pentium-166 and stream say perfect quality AACplusV2 without
any significant performance impact.


What platforms are supported?
=============================
Linux and Win32 at the moment. Want more? Then, err.. port it.


How do I compile it?
====================
Read INSTALL.


How do I configure it?
======================
Read conf/hz.ini.dist. Tune it for your needs.
Prepare your static playlist or write your dynamic playlist handling
function in Lua scripting language. Do not ever mix files that have
different format (MPx/AACx) or different samplerate in one playlist. No,
hz will successfully survive this as well as Icecast, but your player
definitely won't. You've been warned.


How do I run it?
================
hz <hz ini.file>, i.e.:
./hz /etc/hz/rock.ini


Can it be run as daemon on Linux?
=================================
Yes.


As service on Windows?
======================
No. You'll need to use one of that 3rd party software to hide the
hz console window or to install it as service if you have to.


I've found a bug!
=================
Yes, there should be some. Report them, if you care. Oh, and be descriptive
as possible. More details increase the chances for it to be fixed.
Provide no details and your message won't be answered.
