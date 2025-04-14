package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.model.*;
import javafx.scene.control.Label;
import javafx.scene.image.ImageView;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class GameUtils {

    public static final String TITLE = "Star Wars: Rebellion";
    public static final String GAME_ERROR = "Game error";

    @Getter @Setter
    private static int currentTurn = 0;
    @Getter @Setter
    private static int rebelReputation = 13;
    @Getter @Setter
    private static List<ImageView> timePositions;
    @Getter @Setter
    private static boolean secretBaseSelected = false;
    @Getter @Setter
    private static List<Planet> planets;

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

    public static List<Planet> generatePlanets() {
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

    public static void restoreGameState(GameState gameState) {
        planets.forEach(planet -> planet.getLeaders().clear());

        GameState.setSecretBaseLocationStatic(gameState.getSecretBaseLocation());
        currentTurn = gameState.getCurrentTurn();
        rebelReputation = gameState.getRebelReputation();
        GameState.setRebelLeadersStatic(gameState.getRebelLeaders());
        secretBaseSelected = true;

        gameState.getRebelLeaders().forEach(leader -> planets.stream()
                .filter(planet -> planet.getName().equals(leader.getLocation().getName()))
                .forEach(planet -> {
                    if (planet.getLeaders().stream().noneMatch(l -> l.getName().equals(leader.getName()))) {
                        planet.getLeaders().add(leader);
                    }
                })
        );
    }

    public static void nextTurn() {
        if (currentTurn == timePositions.size() - 1) {
            return; // game over
        }
        currentTurn++;
        ImageUtils.setImage(timePositions.get(currentTurn), ImageUtils.TIME_TRACKER_IMAGE);
        timePositions.get(currentTurn - 1).setImage(null);

        if (Boolean.TRUE.equals(GameUtils.gameOver())) {
            // game over
        }
    }

    public static Leader initiateCombat(Planet planet) {
        GameState.setSearchingPlanetStatic(planet);
        Leader removedLeader = null;
        if (Faction.REBELLION.equals(CombatUtils.doCombat(planet.getLeaders()))) {
            SceneUtils.showInformationDialog(
                    "Mission fail!",
                    "You lost.",
                    "The empire lost this combat mission."
            );
            reputationDown();
        }
        else {
            SceneUtils.showInformationDialog(
                    "Mission success!",
                    "You won, the rebel leader is captured.",
                    "You captured the rebel leader and can now search the planet for rebel base."
            );
            removedLeader = captureLeader(planet);
        }
        return removedLeader;
    }

    private static Leader captureLeader(Planet planet) {
        if (!planet.getLeaders().isEmpty()) {
            for (Iterator<Leader> it = planet.getLeaders().iterator(); it.hasNext(); ) {
                Leader leader = it.next();
                if (leader.getFaction().equals(Faction.REBELLION)) {
                    it.remove();
                    return leader;
                }
            }
        }
        return null;
    }


    private static void reputationDown() {
        rebelReputation--;
        ImageUtils.setImage(timePositions.get(rebelReputation), ImageUtils.REPUTATION_TRACKER_IMAGE);
        timePositions.get(rebelReputation + 1).setImage(null);

        if (Boolean.TRUE.equals(gameOver())) {
            // game over
        }
    }

    public static Boolean gameOver() {
        return currentTurn >= rebelReputation;
    }
}
