module Main where
import Text.Printf (printf)
import Debug.Trace

type Latitude  = Double
type Longitude = Double
type Altitude  = Double
data Coordinate = Coordinate Latitude Longitude Altitude deriving (Show, Eq, Ord)
data AzElCord   = AzelCord { azimuth :: Double, elevation :: Double, range :: Double } deriving (Show)

lookAngle :: Coordinate -> Coordinate -> AzElCord
lookAngle (Coordinate gLat gLon gAlt) (Coordinate pLat pLon pAlt) =
  AzelCord azimuth' elevation' range'
  where
    earthRadius  = 6378137
    groundRadius = earthRadius + gAlt
    pointRadius  = earthRadius + pAlt
    gLat'        = (pi / 180) * gLat
    gLon'        = (pi / 180) * gLon
    pLat'        = (pi / 180) * pLat
    pLon'        = (pi / 180) * pLon

    -- WGS84-specific
    wgs84F   = 1 / 298.257223563
    wgs84Ecc = 8.1819190842621E-2
    wgs84N   = earthRadius / sqrt (1 - (wgs84Ecc ** 2) * ((sin gLat') ** 2))

    -- WGS84 -> ECR
    gX = (wgs84N + gAlt) * cos gLat' * cos gLon'
    gY = (wgs84N + gAlt) * cos gLat' * sin gLon'
    gZ = (wgs84N * (1 - wgs84Ecc ** 2) + gAlt) * sin gLat'

    pX = (wgs84N + pAlt) * cos pLat' * cos pLon'
    pY = (wgs84N + pAlt) * cos pLat' * sin pLon'
    pZ = (wgs84N * (1 - wgs84Ecc ** 2) + pAlt) * sin pLat'

    rangeX = pX - gX
    rangeY = pY - gY
    rangeZ = pZ - gZ

    -- Topocentric Horizon
    rotS = sin gLat' * cos gLon' * rangeX + sin gLat' * sin gLon' * rangeY - cos gLat' * rangeZ
    rotE = -(sin gLon' * rangeX) + cos gLon' * rangeY
    rotZ = cos gLat' * cos gLon' * rangeX + cos gLat' * sin gLon' * rangeY + sin gLat' * rangeZ

    range' = sqrt $ (rotS ** 2) + (rotE ** 2) + (rotZ ** 2)

    elRad = if range' == 0
             then pi / 2
             else asin (rotZ / range')

    azRad' = (if rotS == 0
             then pi / 2
             else atan (-(rotE / rotS))) + (if rotS > 0
                                            then pi
                                            else 0)

    azRad = azRad' + if azRad' < 0
                     then (2 * pi)
                     else 0

    elevation' = elRad * (180 / pi)
    azimuth'   = azRad * (180 / pi)

main :: IO ()
main =
  printf "\n\nAz: %.2f\nEl: %.2f\nRange: %.2f\n\n" (azimuth lookAngles) (elevation lookAngles) (range lookAngles)
  where
    ground     = Coordinate 41.09471 (-80.74699) 350.9
    obs        = Coordinate 41.09472 (-80.74697) 351.8
    lookAngles = lookAngle ground obs
