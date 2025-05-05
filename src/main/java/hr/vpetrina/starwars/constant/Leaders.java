package hr.vpetrina.starwars.constant;

import hr.vpetrina.starwars.model.Faction;
import hr.vpetrina.starwars.model.Health;
import hr.vpetrina.starwars.model.Leader;
import hr.vpetrina.starwars.model.Stats;

import java.util.ArrayList;
import java.util.List;

public class Leaders {

    private Leaders() {}

    public static List<Leader> getRebelLeaders() {
        return new ArrayList<>(List.of(
                new Leader(
                        "Luke Skywalker",
                        Faction.REBELLION,
                        new Stats(3, 2),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/luke_skywalker.jpg"
                ),
                new Leader(
                        "Leia Organa",
                        Faction.REBELLION,
                        new Stats(1, 3),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/leia_organa.jpg"
                ),
                new Leader(
                        "Han Solo",
                        Faction.REBELLION,
                        new Stats(3, 1),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/han_solo.png"
                ),
                new Leader(
                        "Jan Donna",
                        Faction.REBELLION,
                        new Stats(0, 4),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/jan_dodonna.png"
                )
        ));
    }

    public static List<Leader> getEmpireLeaders() {
        return new ArrayList<>(List.of(
                new Leader(
                        "Boba Fett",
                        Faction.EMPIRE,
                        new Stats(3, 1),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/boba_fett.jpg"
                ),
                new Leader(
                        "Darth Sidious",
                        Faction.EMPIRE,
                        new Stats(2, 3),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/darth_sidious.jpeg"
                ),
                new Leader(
                        "Darth Vader",
                        Faction.EMPIRE,
                        new Stats(3, 2),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/darth_vader.jpeg"
                ),
                new Leader(
                        "General Tagge",
                        Faction.REBELLION,
                        new Stats(0, 4),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/general_tagge.png"
                )
        ));
    }
}
