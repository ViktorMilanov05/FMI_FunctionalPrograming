main :: IO()
main = do
    print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Bulgaria"
    print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 500 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "France" -- my test

getCapitalElevation :: [City] -> Capital -> Elevation
getCapitalElevation ((City name elevation _):cities) capital
 | name == capital = elevation
 | otherwise = getCapitalElevation cities capital

highestCapital :: [Country] -> Name
highestCapital = fst . foldl1 mostElevated . map capitalElevation
  where
    capitalElevation (Country name capital cities) = (name, getCapitalElevation cities capital)
    mostElevated a@(n1, t1) b@(n2, t2)
      | t1 >= t2 = a
      | otherwise = b


type Name = String
type Capital = Name 
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

