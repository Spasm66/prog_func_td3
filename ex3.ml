(*ex 3.1*)
let combine (x, y) f = f x y

let util x y p = if p x y then combine (x,y) (fun u v -> (v,u)) else x,y

(*ex 3.3*)
let x = 2, true, "ok"
let y = (3, false), "sam"
let z = 12, (true, "mas")

(*ex 3.4*)
type carburant = Diesel | Essence | Electrique
type vehicule = Vehicule of carburant * int
let clio = Vehicule(Diesel, 4)
let make_vehicule c n = Vehicule(c, n)
let carburant_of v = fst v
let nb_weels_of v = snd v
let can_run v = if carburant_of v = Diesel && nb_weels_of v >= 4
    then false else true
let consommation v n = let p_km = match carburant_of v with
    | Diesel -> (6. *. 1.5) /. 100.
    | Essence -> (8. *. 1.5) /. 100.
    | Electrique -> (10. *. 0.25) /. 100.
    in p_km *. n

(*ex 3.5*)
type date = {
    day: int;
    month: int;
    year: int;
}
let today = {day = 22; month = 09; year = 2026}
let date_infeg date1 date2 = if date1.year > date2.year 
    then false
    else if date1.month > date2.month 
        then false
    else date1.day < date2.day

(*ex 3.6*)