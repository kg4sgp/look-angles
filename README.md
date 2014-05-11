Look Angle Calculators
======================

These set of programs calculate look angles from one set of coordnates to the next. This is extremely useful for automatic antenna tracking to moving transmitters with telemetry. For example my inteneded use ofthese is with automatic antenna pointing to a high altitude balloon.

The octave script (the lookangle.m) should be able to be used in Matlab but I have not tried to use it in matlab, only octave.

These programs have been written to take longitude and latitude decimal degrees and altitude in meters.

Currently these programs assume the earth is spherical whereas GPS gives coordinates using the WGS-84 geoid. I plan to change that in the future (even though it probably wont change much anything for normal uses of these programs... I like to be as accurate as I can).
