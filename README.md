Look Angle Calculators
======================

These set of programs calculate look angles from one set of coordinates to the next. This is extremely useful for automatic antenna tracking to moving transmitters with telemetry. For example my intended use of these is with automatic antenna pointing to a high altitude balloon.

The octave script (the lookangle.m) should be able to be used in Matlab but I have not tried to use it in matlab, only octave.

These programs have been written to take longitude and latitude in decimal degrees and altitude in meters.

These programs have been written with different geodetic systems in mind, described by the directory they are in. If you're using GPS data, use the wsg84 programs (GPS uses the WSG 84 geodetic system).

### Notes on using these for antenna pointing ###

If you're using this for antenna pointing you'll probably want to be careful about some situations you might run into. 

Some of these include:

These will indicate an azimuth of 90 when the station being observed is directly overhead. If you're coming from some other azimuth and the station is passing directly over head you might not want to try and go through the entire azimuth to 90 and then down whatever azimuth it leaves that for. The way I'd solve this is to not care about the azimuth results if the station gets close to 90.

If you're really close to the observed station your antenna pointer will probably be bouncing around in its readings. The way I'd solve this is to recognize when the range is below some value and average the readings. In this way we're not moving around all of the time but we're still trying to point in the correct direction. 

Really the way to solve these situations is application specific so enjoy!

KG4SGP - Jim
