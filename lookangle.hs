module Main where

import Text.Printf (printf)

data AzElCord = AzelCord { azimuth :: Float, elevation :: Float, range :: Float }
data GeogCord = GeogCord { latitude :: Float, longitude :: Float, altitude :: Float }

lookAngle :: GeogCord -> GeogCord -> AzElCord
lookAngle g o =
    let
        earthRadius = 6378135
        groundRadius = earthRadius + altitude g
        obsRadius = earthRadius + altitude o

        groundLat = (pi / 180) * latitude g
        groundLon = (pi / 180) * longitude g
        obsLat = (pi / 180) * latitude o
        obsLon = (pi / 180) * longitude o

        groundZ = groundRadius * sin groundLat
        groundR = groundRadius * cos groundLat
        groundX = groundR * cos groundLon
        groundY = groundR * sin groundLon

        obsZ = obsRadius * sin obsLat
        obsR = obsRadius * cos obsLat
        obsX = obsR * cos obsLon
        obsY = obsR * sin obsLon

        vXRange = obsX - groundX
        vYRange = obsY - groundY
        vZRange = obsZ - groundZ

        rotS = sin groundLat * cos groundLon * vXRange + sin groundLat * sin groundLon * vYRange - cos groundLat * vZRange
        rotE = -(sin groundLon * vXRange + cos groundLon * vYRange)
        rotZ = cos groundLat * cos groundLon * vXRange + cos groundLat * sin groundLon * vYRange + sin groundLat * vZRange
        range' = sqrt $ (rotS ** 2) + (rotE ** 2) + (rotZ ** 2)

        el = if range' == 0
             then pi / 2
             else asin $ rotZ / range'

        az = if rotS == 0
             then pi / 2
             else asin $ -(rotE / rotS)
    in
      AzelCord (az * (180 / pi)) (el * (180 / pi)) range'

main :: IO ()
main =
  printf "\n\nAz: %.2f\nEl: %.2f\nRange: %.2f\n\n" (azimuth lookAngles) (elevation lookAngles) (range lookAngles)
  where
    ground     = GeogCord 0 0 0
    obs        = GeogCord 0 90 100000
    lookAngles = lookAngle ground obs
