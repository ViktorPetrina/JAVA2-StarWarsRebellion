package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.controller.MainBoardController;
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

    private static Integer leadersAdded = 0;

    public static void addLeaderToPlanet(int leaderIndex, Planet planet) {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);

        var gameMove = new GameMove(planet, MoveType.DEPLOY, GameState.getFactionTurnStatic());
        var leader = GameState.getPlayerLeadersStatic().get(leaderIndex);

        if (planets.stream().anyMatch(p -> p.getLeaders().contains(leader))) {
            SceneUtils.showInformationDialog(
                    "Planet leaders",
                    "Cannot deploy leader!",
                    "The leader is already deployed, you have to wait for the next turn."
            );
            return;
        }

        if (planet.getLeaders().stream().noneMatch(l -> l.getName().equals(leader.getName()))) {
            gameMove.getLeaders().add(leader);
        }

        if (GameState.getRebelLeadersStatic().stream().noneMatch(l -> l.getName().equals(leader.getName()))) {
            GameState.getRebelLeadersStatic().add(leader);
        }

        leader.setLocation(planet);
        leadersAdded++;

        gameMove.getLeaders().add(leader);
        XmlUtils.saveNewMove(gameMove);

        if (leadersAdded == 2) {
            nextTurn();
            sendUniversalRequest(GameState.getGameState());
        }
    }

    public static void sendUniversalRequest(GameState gameState) {
        if (GameState.getPlayerFactionStatic().equals(Faction.EMPIRE)) {
            NetworkUtils.sendRequestPlayerTwo(gameState);
        }
        else {
            NetworkUtils.sendRequestPlayerOne(gameState);
        }
    }

    public static void restoreGameState(GameState gameState) {
        planets.forEach(planet -> planet.getLeaders().clear());

        GameState.setSecretBaseLocationStatic(gameState.getSecretBaseLocation());
        GameState.setRebelLeadersStatic(gameState.getRebelLeaders());
        currentTurn = gameState.getCurrentTurn();
        rebelReputation = gameState.getRebelReputation();
        secretBaseSelected = true;

        gameState.getRebelLeaders().forEach(leader -> planets.stream()
                .filter(planet -> planet.getName().equals(leader.getLocation().getName()))
                .forEach(planet -> {
                    if (planet.getLeaders().stream().noneMatch(l -> l.getName().equals(leader.getName()))) {
                        planet.getLeaders().add(leader);
                    }
                })
        );

        timePositions.forEach(tp -> tp.setImage(null));
        ImageUtils.setImage(timePositions.get(currentTurn), ImageUtils.TIME_TRACKER_IMAGE);
        ImageUtils.setImage(timePositions.get(rebelReputation), ImageUtils.REPUTATION_TRACKER_IMAGE);
    }

    public static void nextTurn() {
        LogUtils.logInfo("Next turn! (Current turn: " + (currentTurn + 1) + ")");

        leadersAdded = 0;

        if (currentTurn == timePositions.size() - 1) {
            return; // game over
        }

        currentTurn++;
        ImageUtils.setImage(timePositions.get(currentTurn), ImageUtils.TIME_TRACKER_IMAGE);
        timePositions.get(currentTurn - 1).setImage(null);
    }

    public static Leader captureLeader(Planet planet, Faction faction) {
        if (!planet.getLeaders().isEmpty()) {
            for (Iterator<Leader> it = planet.getLeaders().iterator(); it.hasNext(); ) {
                Leader leader = it.next();
                if (leader.getFaction().equals(faction)) {
                    it.remove();
                    return leader;
                }
            }
        }
        return null;
    }

    public static void reputationDown() {
        rebelReputation--;
        ImageUtils.setImage(timePositions.get(rebelReputation), ImageUtils.REPUTATION_TRACKER_IMAGE);
        timePositions.get(rebelReputation + 1).setImage(null);
    }

    public static boolean hasEmpireLeader(Planet planet) {
        return planet.getLeaders().stream().anyMatch(leader -> leader.getFaction().equals(Faction.EMPIRE));
    }

    public static Boolean gameOver() {
        return currentTurn >= rebelReputation;
    }
}
