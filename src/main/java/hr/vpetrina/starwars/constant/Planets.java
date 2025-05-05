package hr.vpetrina.starwars.constant;

import hr.vpetrina.starwars.model.ControlStatus;
import hr.vpetrina.starwars.model.Planet;

import java.util.ArrayList;
import java.util.List;

public class Planets {

    private Planets() {}

    public static List<Planet> getPlanets() {
        return new ArrayList<>(List.of(
                new Planet("Naboo", ControlStatus.NEUTRAL, new ArrayList<>()),
                new Planet("Kashyyyk", ControlStatus.NEUTRAL, new ArrayList<>()),
                new Planet("Tatooine", ControlStatus.NEUTRAL, new ArrayList<>()),
                new Planet("Hoth", ControlStatus.NEUTRAL, new ArrayList<>()),
                new Planet("Coruscant", ControlStatus.NEUTRAL, new ArrayList<>()),
                new Planet("Mustafar", ControlStatus.NEUTRAL, new ArrayList<>()),
                new Planet("Ilum", ControlStatus.NEUTRAL, new ArrayList<>())
        ));
    }
}
