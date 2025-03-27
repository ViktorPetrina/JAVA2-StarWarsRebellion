package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.model.Faction;
import hr.vpetrina.starwars.model.GameState;
import hr.vpetrina.starwars.model.Leader;
import hr.vpetrina.starwars.model.Planet;
import hr.vpetrina.starwars.util.*;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.Pane;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class MainBoardController {

    @FXML
    public Label lblMessage;

    @FXML
    public ImageView imgPlanetKashyyyk;
    @FXML
    public ImageView imgPlanetTatooine;
    @FXML
    public ImageView imgPlanetCoruscant;
    @FXML
    public ImageView imgPlanetNaboo;
    @FXML
    public ImageView imgPlanetMustafar;
    @FXML
    public ImageView imgPlanetHoth;
    @FXML
    public ImageView imgPlanetIlum;

    @FXML
    public ImageView leaderImage1;
    @FXML
    public Label leaderName1;
    @FXML
    public Button btnLeader1;
    @FXML
    public ImageView leaderImage2;
    @FXML
    public Label leaderName2;
    @FXML
    public Button btnLeader2;

    @FXML
    public ImageView timePositionOne;
    @FXML
    public ImageView timePositionThree;
    @FXML
    public ImageView timePositionFive;
    @FXML
    public ImageView timePositionSeven;
    @FXML
    public ImageView timePositionNine;
    @FXML
    public ImageView timePositionEleven;
    @FXML
    public ImageView timePositionThirteen;
    @FXML
    public ImageView timePositionFifteen;
    @FXML
    public ImageView timePositionTwo;
    @FXML
    public ImageView timePositionFour;
    @FXML
    public ImageView timePositionSix;
    @FXML
    public ImageView timePositionEight;
    @FXML
    public ImageView timePositionTwelve;
    @FXML
    public ImageView timePositionTen;
    @FXML
    public ImageView timePositionFourteen;
    @FXML
    public ImageView timePositionSixteen;

    @FXML
    public Label leaderStat11;
    @FXML
    public Label leaderStat12;
    @FXML
    public Label leaderStat21;
    @FXML
    public Label leaderStat22;

    @FXML
    private Pane menuPane;
    @FXML
    private Pane planetMenuPane;

    @FXML
    public Label lblPlanetName;
    @FXML
    public Label lblPlanetControl;
    @FXML
    public Label lblPlanetLeaders;

    @FXML
    private Pane mainPane;

    private List<ImageView> planetImages;
    private static List<Planet> planets;
    private List<ImageView> timePositions;

    private static int currentTurn = 0;
    private static int rebelReputation = 13;
    private Boolean menuOpened = false;
    private static Boolean secretBaseSelected = false;
    private static Planet selectedPlanet;
    private Leader selectedLeader;

    public static void disableControls(boolean b) {

    }

    @FXML
    public void initialize() {
        initializePlanets();
        initializeTimePositions();
        initializeMenu();
        initializeSecretBase();
        initializeEventListeners();
        GameUtils.initializeLeaders(
                GameState.getPlayerLeaders(),
                List.of(leaderName1, leaderName2),
                List.of(leaderImage1, leaderImage2),
                getStats()
        );
    }

    private void initializePlanets() {
        planetImages = List.of(
                imgPlanetTatooine,
                imgPlanetCoruscant,
                imgPlanetNaboo,
                imgPlanetMustafar,
                imgPlanetHoth,
                imgPlanetIlum,
                imgPlanetKashyyyk
        );

        planets = GameUtils.getPlanets();
    }

    private void initializeEventListeners() {
        if (GameState.getPlayerFaction() == Faction.EMPIRE || Boolean.TRUE.equals(secretBaseSelected)) {
            planetImages.forEach(planet -> planet.setOnMouseClicked(_ -> openPlanetMenu(planet)));
        }

        mainPane.requestFocus();

        btnLeader1.setOnAction(_ -> {
            var leader = GameState.getPlayerLeaders().getFirst();
            selectedPlanet.getLeaders().add(leader);
            leader.setLocation(selectedPlanet);
            GameState.setRebelLeadersStatic(List.of(leader));
            NetworkUtils.sendRequestPlayerOne(GameState.getGameState());
        });
        btnLeader2.setOnAction(_ -> selectedPlanet.getLeaders().add(GameState.getPlayerLeaders().get(1)));
    }

    private void openPlanetMenu(ImageView planetImage) {
        selectPlanet(planetImage);
        lblPlanetName.setText(selectedPlanet.getName());
        lblPlanetControl.setText(selectedPlanet.getControlStatus().toString());

        StringBuilder sb = new StringBuilder();
        selectedPlanet.getLeaders().forEach(leader -> sb.append(leader.getName()).append(" | "));

        lblPlanetLeaders.setText(sb.toString());

        planetMenuPane.setVisible(true);
        SoundUtils.playSound(SoundUtils.MENU_SOUND);
    }

    private void initializeSecretBase() {
        if (GameState.getPlayerFaction() != Faction.REBELLION) {
            return;
        }

        lblMessage.setText("Choose a location for your secret base!");
        planetImages.forEach(planet -> planet.setOnMouseClicked(_ -> {
            selectPlanet(planet);
            SoundUtils.playSound(SoundUtils.READY_SOUND);
            if (selectedPlanet != null) {
                GameState.setSecretBaseLocationStatic(selectedPlanet);
                lblMessage.setText("Secret base selected! Location: " + selectedPlanet.getName());
                secretBaseSelected = true;
            }

            initializeEventListeners();
            GameState.setFactionTurnStatic(Faction.EMPIRE);
            NetworkUtils.sendRequestPlayerOne(GameState.getGameState());
        }));
    }

    private void selectPlanet(ImageView planet) {
        selectedPlanet = planets
                .stream()
                .filter(p -> p.getName().equals(planet.getUserData().toString()))
                .toList()
                .getFirst();
    }

    private void initializeMenu() {
        menuPane.setVisible(false);
        planetMenuPane.setVisible(false);
    }

    @FXML
    private void keyPressed(KeyEvent keyEvent) {
        if (Objects.requireNonNull(keyEvent.getCode()) == KeyCode.ESCAPE) {
            openOrCloseMenu();
        }
    }

    private void openOrCloseMenu() {
        SoundUtils.playSound(SoundUtils.MENU_SOUND);
        if (Boolean.FALSE.equals(menuOpened)) {
            menuPane.setVisible(true);
            menuOpened = true;
        }
        else {
            menuPane.setVisible(false);
            menuOpened = false;
        }
    }

    @FXML
    private void showChat() {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);
    }

    @FXML
    private void generateDocumentation() {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);
        try {
            DocumentationUtils.generateDocumentation();
            SceneUtils.showInformationDialog(
                    "Documentation",
                    "Documentation generated",
                    "The documentation is successfully generated!"
            );
        } catch (IOException e) {
            SceneUtils.showWarningDialog(
                    "Documentation error",
                    "The documentation cannot be generated",
                    "Something went wrong while generating html documentation."
            );
        }
    }

    private void initializeTimePositions() {
        timePositions = List.of(
                timePositionOne, timePositionTwo, timePositionThree, timePositionFour,
                timePositionFive, timePositionSix, timePositionSeven, timePositionEight,
                timePositionNine, timePositionTen, timePositionEleven, timePositionTwelve,
                timePositionThirteen, timePositionFourteen, timePositionFifteen, timePositionSixteen
        );

        ImageUtils.setImage(timePositions.get(currentTurn), ImageUtils.TIME_TRACKER_IMAGE);
        ImageUtils.setImage(timePositions.get(rebelReputation), ImageUtils.REPUTATION_TRACKER_IMAGE);
    }


    private void nextTurn() {
        if (currentTurn == timePositions.size() - 1) {
            return; // game over
        }
        currentTurn++;
        ImageUtils.setImage(timePositions.get(currentTurn), ImageUtils.TIME_TRACKER_IMAGE);
        timePositions.get(currentTurn - 1).setImage(null);

        if (GameUtils.gameOver()) {
            // game over
        }
    }

    private void reputationUp() {
        GameState.setCurrentTurnStatic(GameState.getCurrentTurnStatic() + 1);
        rebelReputation++;
        ImageUtils.setImage(timePositions.get(rebelReputation), ImageUtils.REPUTATION_TRACKER_IMAGE);
        timePositions.get(rebelReputation - 1).setImage(null);

        if (GameUtils.gameOver()) {
            // game over
        }
    }

    private void reputationDown() {
        rebelReputation--;
        ImageUtils.setImage(timePositions.get(rebelReputation), ImageUtils.REPUTATION_TRACKER_IMAGE);
        timePositions.get(rebelReputation + 1).setImage(null);

        if (GameUtils.gameOver()) {
            // game over
        }
    }

    private List<List<Label>> getStats() {
        List<Label> leader1Stats = Arrays.asList(leaderStat11, leaderStat12);
        List<Label> leader2Stats = Arrays.asList(leaderStat21, leaderStat22);
        return Arrays.asList(leader1Stats, leader2Stats);
    }

    public static void restoreGameState(GameState gameState) {
        GameState.setSecretBaseLocationStatic(gameState.getSecretBaseLocation());
        GameState.setCurrentTurnStatic(gameState.getCurrentTurn());
        GameState.setRebelReputationStatic(gameState.getRebelReputation());
        GameState.setRebelLeadersStatic(gameState.getRebelLeaders());
        secretBaseSelected = true;
        currentTurn = gameState.getCurrentTurn();
        rebelReputation = gameState.getRebelReputation();

        gameState.getRebelLeaders().forEach(leader -> planets.stream()
                .filter(planet -> planet.getName().equals(leader.getLocation().getName()))
                .forEach(planet -> planet.getLeaders().add(leader)));
    }

    @FXML
    public void searchPlanet() {
        if (selectedPlanet == null) {
            return;
        }

        if (Faction.EMPIRE.equals(GameState.getPlayerFaction())) {
            searchPlanetEmpire();
        }
        else {
            searchPlanetRebellion();
        }
    }

    private void searchPlanetRebellion() {

    }

    private static void searchPlanetEmpire() {
        if (!selectedPlanet.getLeaders().isEmpty()) {
            GameState.setSearchingPlanetStatic(selectedPlanet);
            SceneUtils.showInformationDialog(
                    "Combat!",
                    "Combat",
                    "Combat"
            );
        }

        if (selectedPlanet.getName().equals(GameState.getSecretBaseLocationStatic().getName())) {
            SceneUtils.showInformationDialog(
                    "Success!",
                    "Rebel base found.",
                    "You have found secret rebel base and won the game!"
            );
        }
        else {
            SceneUtils.showInformationDialog(
                    "Fail!",
                    "No rebel base found.",
                    "You have not found secret rebel base and haven't won the game!"
            );
        }
    }

    @FXML
    public void attackPlanet() {
        if (selectedPlanet == null) {
            return;
        }
    }

    @FXML
    private void exitGame() {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);
        Platform.exit();
    }
}