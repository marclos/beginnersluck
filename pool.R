## Calculating Heat Budget for Pool

#Dimensions (in feet)

Length = 35
Width = 25
Depth = 8

Volume_ft = Length * Width * Depth
Volume_m = Volume_ft * 0.0283168

Water_mass = Volume_m * 1000

Current_temp_F = 70
Goal_temp_F = 86

Current_temp_C = (Current_temp_F  - 32) * 5/9
Goal_temp_C = (Goal_temp_F - 32) * 5/9

Delta_temp = Goal_temp_C - Current_temp_C

joules_per_kg_per_degreesC = 4186

Joules_needed = Delta_temp * Water_mass * joules_per_kg_per_degreesC

therms_per_Joule = 9.4708628903179*10^-9
Therms_needed = Joules_needed * therms_per_Joule

Therms_needed

Dollars_per_therm = 1.20

Dollars = Therms_needed * Dollars_per_therm; Dollars

Generally, evaporation account for 30 to 50% of the total heat loss. In the middle of summer, mid-size pools typically lose about 50 mm of water per week in this way, the equivalent of 150 kilowatts-hours. This phenomenon is accelerating in dry or windy. The frequent use of a pool cover significantly reduce evaporation losses.

Here is the formula for evaporation losses

Pevapo. = (25+19Vw) x S x (X - X') x Lv x t/1000
                           
with:
                           
\Pevapo: evaporation losses (kWh)
                           
25 + 19Vv: empirical formula giving the ratio of the heat exchange coefficient hconv by convection between the water and air (W / m²K) on the Cair specific heat of air (= 0,277Wh / kg of air.K)
                           
                           Vw: wind speed (m / s)
                           
                           S: the pool surface (m²)
                           
                           X: specific humidity of saturated air at the temperature of the water (g water / kg air)
                           
                           X ': specific humidity of ambient air at the temperature of the water (g water / kg air)

Lv: enthalpy (or latent heat) of vaporization of water: 0.625 Wh / g

t: time without solar cover (in hours)

With my best regards

Prof. Bachir ACHOUR
