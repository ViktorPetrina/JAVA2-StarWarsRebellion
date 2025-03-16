package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.model.Faction;
import hr.vpetrina.starwars.model.Health;
import hr.vpetrina.starwars.model.Leader;
import hr.vpetrina.starwars.model.Stats;
import javafx.scene.control.Label;
import javafx.scene.image.ImageView;

import java.util.ArrayList;
import java.util.List;

public class GameUtils {

    public static final String TITLE = "Star Wars: Rebellion";
    public static final String GAME_ERROR = "Game error";

    private GameUtils() {}

    public static void initializeLeaders(List<Leader> leaders, List<Label> labels, List<ImageView> images) {
        for (int i = 0; i < leaders.size(); i++) {
            labels.get(i).setText(leaders.get(i).getName());
            SceneUtils.setImage(images.get(i), leaders.get(i).getImagePath());
        }
    }

    public static List<Leader> getRebelLeaders() {
        return new ArrayList<>(List.of(
                new Leader(
                        "Luke Skywalker",
                        Faction.REBELLION,
                        new Stats(0, 0, 0, 0),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/luke_skywalker.jpg"
                ),
                new Leader(
                        "Leia Organa",
                        Faction.REBELLION,
                        new Stats(0, 0, 0, 0),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/leia_organa.jpg"
                ),
                new Leader(
                        "Han Solo",
                        Faction.REBELLION,
                        new Stats(0, 0, 0, 0),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/han_solo.png"
                ),
                new Leader(
                        "Jan Donna",
                        Faction.REBELLION,
                        new Stats(0, 0, 0, 0),
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
                        new Stats(0, 0, 0, 0),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/boba_fett.jpg"
                ),
                new Leader(
                        "Darth Sidious",
                        Faction.EMPIRE,
                        new Stats(0, 0, 0, 0),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/darth_sidious.jpeg"
                ),
                new Leader(
                        "Darth Vader",
                        Faction.EMPIRE,
                        new Stats(0, 0, 0, 0),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/darth_vader.jpeg"
                ),
                new Leader(
                        "General Tagge",
                        Faction.REBELLION,
                        new Stats(0, 0, 0, 0),
                        Health.ALIVE,
                        null,
                        false,
                        "/hr/vpetrina/starwars/images/general_tagge.png"
                )
        ));
    }
}
