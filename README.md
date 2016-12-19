# NetCommand: a remote scripting mod for Minecraft 1.8+.
Uses Forge, tested with Minecraft 1.11+.

# Why?

I wanted to play around a bit with Scala!  But also, have you ever gotten tired of building long walls in Minecraft?  Or painstakingly putting in a checkerboard pattern?  Or recreating a rocket that uses pistons, switching back and forth around your inventory?  Or creating a REALLY high tower of sand?  If only there was a way to automate the creation of YOUR creations...

Well, now there is.  This mod opens up a TCP listener (port 4321) and waits for commands from some external source.  The commands themselves are very simple - get a block, place a block, check where the player is, set the origin of a creation, and so on - and don't use any "special" protocol.  You can type them in using Netcat, if you'd like; see the `parse` function.  But a better way would be to write a program that issues the appropriate instructions, thus scripting Minecraft remotely :).

# TODO

Since this was an exercise in learning about and playing with Scala (which is a nice enough language, I've found!), I haven't invested a lot of effort into backward- and forward-compatibility with Minecraft versions.  I also haven't made a JAR that can be dropped into a Mods folder ... if anyone wants one, please just ask through the Issues system on GitHub, and I'll get to it.  It's roughly 300 lines, including comments and debugging, so it shouldn't be too difficult to wade through it and modify it as Minecraft develops.
