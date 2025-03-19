package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.model.*;
import javafx.scene.control.Label;
import javafx.scene.image.ImageView;

import java.util.ArrayList;
import java.util.List;

public class GameUtils {

    public static final String TITLE = "Star Wars: Rebellion";
    public static final String GAME_ERROR = "Game error";

    private GameUtils() {}

    public static void initializeLeaders(
            List<Leader> leaders,
            List<Label> labels,
            List<ImageView> images,
            List<List<Label>> stats
    ) {
        for (int i = 0; i < leaders.size(); i++) {
            labels.get(i).setText(leaders.get(i).getName());
            ImageUtils.setImage(images.get(i), leaders.get(i).getImagePath());
            stats.get(i).get(0).setText(leaders.get(i).getSkills().getCombat().toString());
            stats.get(i).get(1).setText(leaders.get(i).getSkills().getEspionage().toString());
        }
    }

    public static void initializeLeadersSimple(
            List<Leader> leaders,
            List<Label> labels,
            List<ImageView> images
    ) {
        for (int i = 0; i < leaders.size(); i++) {
            labels.get(i).setText(leaders.get(i).getName());
            ImageUtils.setImage(images.get(i), leaders.get(i).getImagePath());
        }
    }

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
