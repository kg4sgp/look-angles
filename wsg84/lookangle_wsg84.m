% Jimmy Carter, KG4SGP
% Look angle calculator
% BSD 3 Clause License at bottom

% This function takes two coordinates (longitude, latitude, altitude) 
% and gives the look angle from the first (ground) to the second (point)
% along with the range to the second.

% Longitude and Latitude in decimal degrees (ddd.ddddddd)
% Altitude in meters

function [az, el, range] = lookangle_wsg84(lat_ground, lon_ground, alt_ground, lat_point, lon_point, alt_point);

	% Add the radius of the earth to the heights
	radius_e = 6378137;
	radius_ground = radius_e + alt_ground;
	radius_point = radius_e + alt_point;

	% Convert from decimal degrees to radians
	lat_ground = (pi/180)*lat_ground;
	lon_ground = (pi/180)*lon_ground;
	lat_point = (pi/180)*lat_point;
	lon_point = (pi/180)*lon_point;

	% WSG84 
	f = 1/298.257223563;
	ecc = 8.1819190842621E-2;
	N = radius_e / sqrt(1 - (ecc.^2)*sin(lat_ground).^2);

	% WSG84 to ECR
	x_ground = (N+alt_ground)*cos(lat_ground)*cos(lon_ground);
	y_ground = (N+alt_ground)*cos(lat_ground)*sin(lon_ground);
	z_ground = (N*(1 - ecc.^2) + alt_ground)*sin(lat_ground);

	x_point = (N+alt_point)*cos(lat_point)*cos(lon_point);
	y_point = (N+alt_point)*cos(lat_point)*sin(lon_point);
	z_point = (N*(1 - ecc.^2) + alt_point)*sin(lat_point);
	
	% Calculate the range vector
	range_v = [x_point-x_ground, y_point-y_ground, z_point-z_ground];

	% Transform range vector to Topocentric Horizon
	rot_s = sin(lat_ground)*cos(lon_ground)*range_v(1) + sin(lat_ground)*sin(lon_ground)*range_v(2) - cos(lat_ground)*range_v(3);
	rot_e = -1*sin(lon_ground)*range_v(1) + cos(lon_ground)*range_v(2);
	rot_z = cos(lat_ground)*cos(lon_ground)*range_v(1) + cos(lat_ground)*sin(lon_ground)*range_v(2) + sin(lat_ground)*range_v(3);

	% Calculate range distance
	range = sqrt(rot_s.^2 + rot_e.^2 + rot_z.^2);

	% Calculate elevation and point upwards if range is 0 (same point for all we can tell)
	if(range == 0)
		el_rad = (pi/2);
	else
		el_rad = asin(rot_z/range);
	end

	% Calculate azimuth and take care of divide by zero case
	if(rot_s == 0)
		az_rad = (pi/2);
	else
		az_rad = atan(-1*(rot_e/rot_s));
	end

	if (rot_s > 0)
		az_rad = az_rad+pi;
	end

	if (az_rad < 0)
		az_rad = az_rad+(2*pi);
	end

	% Convert az and el to degrees
	el = el_rad*(180/pi);
	az = az_rad*(180/pi);

end

% Copyright (c) 2014, Jimmy Carter KG4SGP
% All rights reserved.

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%     * Redistributions of source code must retain the above copyright
%       notice, this list of conditions and the following disclaimer.
%     * Redistributions in binary form must reproduce the above copyright
%       notice, this list of conditions and the following disclaimer in the
%       documentation and/or other materials provided with the distribution.
%     * Neither the name of the Jimmy Carter KG4SGP nor the
%       names of its contributors may be used to endorse or promote products
%       derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
% DISCLAIMED. IN NO EVENT SHALL Jimmy Carter KG4SGP BE LIABLE FOR ANY
% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
