package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.model.*;
import javafx.scene.control.Label;
import javafx.scene.image.ImageView;
import lombok.Getter;
import lombok.Setter;

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
            planet.getLeaders().add(leader);
        }

        if (GameState.getRebelLeadersStatic().stream().noneMatch(l -> l.getName().equals(leader.getName()))) {
            GameState.getRebelLeadersStatic().add(leader);
        }

        leader.setLocation(planet);
        leadersAdded++;

        gameMove.getLeaders().add(leader);
        XmlUtils.saveNewMove(gameMove);
        ThreadUtils.saveLastEvent(gameMove);

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

    public static void setSecretBase(Planet planet, Label label) {
        GameState.setSecretBaseLocationStatic(planet);
        label.setText("Secret base location: " + planet.getName());
        secretBaseSelected = true;

        var gameMove = new GameMove();
        gameMove.setExecutor(Faction.REBELLION);
        gameMove.setMoveType(MoveType.SECRET_BASE_PICK);
        gameMove.setPlanet(planet);

        XmlUtils.saveNewMove(gameMove);
        ThreadUtils.saveLastEvent(gameMove);
    }

    public static PlanetSearchResult searchPlanet(Planet planet) {
        if (planet.getLeaders().stream().anyMatch(leader -> leader.getFaction().equals(Faction.REBELLION))) {
            return PlanetSearchResult.IS_PROTECTED;
        }

        if (planet.getName().equals(GameState.getSecretBaseLocationStatic().getName())) {
            var gameMove = new GameMove(planet, MoveType.SEARCH, Faction.EMPIRE, Faction.EMPIRE);
            XmlUtils.saveNewMove(gameMove);
            ThreadUtils.saveLastEvent(gameMove);
            return PlanetSearchResult.HAS_SECRET_BASE;
        }
        else {

            var gameMove = new GameMove(planet, MoveType.SEARCH, Faction.REBELLION, Faction.EMPIRE);
            XmlUtils.saveNewMove(gameMove);
            ThreadUtils.saveLastEvent(gameMove);
            GameUtils.nextTurn();
            return PlanetSearchResult.NO_SECRET_BASE;
        }
    }

    public static Boolean gameOver() {
        return currentTurn >= rebelReputation;
    }
}
