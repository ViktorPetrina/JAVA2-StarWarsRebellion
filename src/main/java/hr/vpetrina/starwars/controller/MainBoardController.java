package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.model.Faction;
import hr.vpetrina.starwars.model.GameState;
import hr.vpetrina.starwars.model.Planet;
import hr.vpetrina.starwars.util.*;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.Pane;

import java.io.IOException;
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
    private List<Planet> planets;
    private List<ImageView> timePositions;

    private int currentTurn = 0;
    private Boolean menuOpened = false;
    private Boolean secretBaseSelected = false;
    private Planet selectedPlanet;
    private Planet secretBaseLocation;

    @FXML
    public void initialize() {
        initializePlanets();
        initializeTimePositions();
        initializeMenu();
        initializeSecretBase();
        initializeEventListeners();
        GameUtils.initializeLeadersSimple(
                GameState.getPlayerLeaders(),
                List.of(leaderName1, leaderName2),
                List.of(leaderImage1, leaderImage2)
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
    }

    private void openPlanetMenu(ImageView planetImage) {
        selectPlanet(planetImage);
        lblPlanetName.setText(selectedPlanet.getName());
        lblPlanetControl.setText(selectedPlanet.getControlStatus().toString());
        //lblPlanetLeaders.setText(selectedPlanet.getLeaders().toString()); ili nesto slicno

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
                secretBaseLocation = selectedPlanet;
                lblMessage.setText("Secret base selected!\nLocation: " + secretBaseLocation.getName());
                secretBaseSelected = true;
            }

            initializeEventListeners();
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
        else if (keyEvent.getCode() == KeyCode.ENTER) {
            nextTurn(); // for testing
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
    private void closePlanetMenu() {
        planetMenuPane.setVisible(false);
        SoundUtils.playSound(SoundUtils.RESET_SOUND);
        mainPane.requestFocus();
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

        ImageUtils.setImage(timePositions.getFirst(), ImageUtils.TIME_TRACKER_IMAGE);
        ImageUtils.setImage(timePositions.get(13), ImageUtils.REPUTATION_TRACKER_IMAGE);
    }


    private void nextTurn() {
        if (currentTurn == timePositions.size() - 1) {
            return; // game over
        }

        currentTurn++;
        ImageUtils.setImage(timePositions.get(currentTurn), ImageUtils.TIME_TRACKER_IMAGE);
        timePositions.get(currentTurn - 1).setImage(null);
    }

    @FXML
    private void exitGame() {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);
        Platform.exit();
    }
}