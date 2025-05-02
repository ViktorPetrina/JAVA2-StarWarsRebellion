package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.model.Faction;
import hr.vpetrina.starwars.model.GameState;
import hr.vpetrina.starwars.model.Leader;
import hr.vpetrina.starwars.model.Planet;
import hr.vpetrina.starwars.rmi.ChatRemoteService;
import hr.vpetrina.starwars.util.*;
import javafx.animation.Timeline;
import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.Pane;

import java.io.IOException;
import java.util.*;

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
    public Pane gameOverPane;
    @FXML
    public Label lblWhoWon;
    @FXML
    public Pane messagePane;
    @FXML
    public Label lblMsgTitle;
    @FXML
    public Label lblMsgContent;
    @FXML
    public Button btnAttackPlanet;
    @FXML
    public Button btnSearchPlanet;

    public static final List<Button> buttons = new ArrayList<>();

    @FXML
    public TextArea chatMessagesTextArea;
    @FXML
    public TextField chatMessageTextField;

    @FXML
    private Pane menuPane;
    @FXML
    private Pane planetMenuPane;

    @FXML
    private Pane chatPane;

    @FXML
    public Label lblPlanetName;
    @FXML
    public Label lblPlanetControl;
    @FXML
    public Label lblPlanetLeaders;

    @FXML
    private Pane mainPane;

    private List<ImageView> planetImages;
    private Boolean menuOpened = false;
    private Boolean chatOpened = false;
    private static Planet selectedPlanet;

    private ChatRemoteService chatRemoteService;

    @FXML
    public void initialize() {
        initializeChatService();
        initializeButtonList();
        initializePlanets();
        initializeTimePositions();
        initializePanes();
        initializeSecretBase();
        initializeEventListeners();
        GameUtils.initializeLeaders(
                GameState.getPlayerLeadersStatic(),
                List.of(leaderName1, leaderName2),
                List.of(leaderImage1, leaderImage2),
                getStats()
        );
    }

    private void initializeChatService() {
        Optional<ChatRemoteService> chatRemoteServiceOptional = ChatUtils.initializeChatRemoteService();
        chatRemoteServiceOptional.ifPresent(remoteService -> {
            chatRemoteService = remoteService;
            Timeline chatMessagesRefreshTimeLine = ChatUtils.getChatRefreshTimeline(chatRemoteService, chatMessagesTextArea);
            chatMessagesRefreshTimeLine.play();
        });
    }

    @FXML
    private void sendMessage() {
        ChatUtils.sendChatMessage(chatRemoteService, chatMessageTextField);
    }

    private void initializeButtonList() {
        buttons.add(btnAttackPlanet);
        buttons.add(btnSearchPlanet);
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

        GameUtils.setPlanets(GameUtils.generatePlanets());
    }

    private void initializeEventListeners() {
        if (GameState.getPlayerFactionStatic() == Faction.EMPIRE || Boolean.TRUE.equals(GameUtils.isSecretBaseSelected())) {
            planetImages.forEach(planet -> planet.setOnMouseClicked(_ -> openPlanetMenu(planet)));
        }
        btnLeader1.setOnAction(_ -> GameUtils.addLeaderToPlanet(0, selectedPlanet));
        btnLeader2.setOnAction(_ -> GameUtils.addLeaderToPlanet(1, selectedPlanet));

        mainPane.requestFocus();
    }

    private static void removeLeaderFromPlanet(Leader leader) {
        selectedPlanet.getLeaders().removeIf(l -> l.getName().equals(leader.getName()));
        GameState.getRebelLeadersStatic().removeIf(l -> l.getName().equals(leader.getName()));
        leader.setLocation(null);
    }

    private void openPlanetMenu(ImageView planetImage) {
        selectPlanet(planetImage);
        lblPlanetName.setText(selectedPlanet.getName());
        lblPlanetControl.setText(selectedPlanet.getControlStatus().toString());

        StringBuilder sb = new StringBuilder();
        selectedPlanet.getLeaders().forEach(leader -> sb.append(leader.getName()).append('\n'));
        lblPlanetLeaders.setText(sb.toString());

        planetMenuPane.setVisible(true);
        SoundUtils.playSound(SoundUtils.MENU_SOUND);
    }

    private void initializeSecretBase() {
        if (GameState.getPlayerFactionStatic() != Faction.REBELLION) {
            return;
        }

        lblMessage.setText("Choose a location for your secret base!");
        planetImages.forEach(planet -> planet.setOnMouseClicked(_ -> {
            selectPlanet(planet);
            SoundUtils.playSound(SoundUtils.READY_SOUND);
            if (selectedPlanet != null) {
                setSecretBase(selectedPlanet, lblMessage);
            }

            initializeEventListeners();
            NetworkUtils.sendRequestPlayerOne(GameState.getGameState());
        }));
    }

    public static void setSecretBase(Planet planet, Label label) {
        GameState.setSecretBaseLocationStatic(planet);
        label.setText("Secret base location: " + planet.getName());
        GameUtils.setSecretBaseSelected(true);
    }

    private void selectPlanet(ImageView planet) {
        selectedPlanet = GameUtils.getPlanets()
                .stream()
                .filter(p -> p.getName().equals(planet.getUserData().toString()))
                .toList()
                .getFirst();
    }

    private void initializePanes() {
        menuPane.setVisible(false);
        planetMenuPane.setVisible(false);
        messagePane.setVisible(false);
        gameOverPane.setVisible(false);
        chatPane.setVisible(false);
    }

    @FXML
    private void keyPressed(KeyEvent keyEvent) {
        if (Objects.requireNonNull(keyEvent.getCode()) == KeyCode.ESCAPE) {
            openOrCloseMenu();
        } else if (keyEvent.getCode() == KeyCode.C) {
            openOrCloseChat();
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
    public void closeMessagePane() {
        messagePane.setVisible(false);
    }

    @FXML
    private void openOrCloseChat() {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);
        if (Boolean.FALSE.equals(chatOpened)) {
            chatPane.setVisible(true);
            chatOpened = true;
        }
        else {
            chatPane.setVisible(false);
            chatOpened = false;
        }
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

    @FXML
    private void saveGame() {
        FileUtils.saveGameState(GameState.getGameState());
    }

    @FXML
    private void loadGame() {
        GameUtils.restoreGameState(FileUtils.loadGameState());
        lblMessage.setText("Secret base location: " + GameState.getSecretBaseLocationStatic().getName());
        initializeEventListeners();
    }

    private void initializeTimePositions() {
        GameUtils.setTimePositions(List.of(
                timePositionOne, timePositionTwo, timePositionThree, timePositionFour,
                timePositionFive, timePositionSix, timePositionSeven, timePositionEight,
                timePositionNine, timePositionTen, timePositionEleven, timePositionTwelve,
                timePositionThirteen, timePositionFourteen, timePositionFifteen, timePositionSixteen
        ));

        ImageUtils.setImage(
                GameUtils.getTimePositions().get(GameUtils.getCurrentTurn()),
                ImageUtils.TIME_TRACKER_IMAGE
        );
        ImageUtils.setImage(
                GameUtils.getTimePositions().get(GameUtils.getRebelReputation()),
                ImageUtils.REPUTATION_TRACKER_IMAGE
        );
    }

    private List<List<Label>> getStats() {
        List<Label> leader1Stats = Arrays.asList(leaderStat11, leaderStat12);
        List<Label> leader2Stats = Arrays.asList(leaderStat21, leaderStat22);
        return Arrays.asList(leader1Stats, leader2Stats);
    }

    @FXML
    public void searchPlanet() {
        if (selectedPlanet == null) {
            return;
        }

        if (Faction.EMPIRE.equals(GameState.getPlayerFactionStatic())) {
            searchPlanetEmpire();
        }
    }

    private void searchPlanetEmpire() {
        if (selectedPlanet.getLeaders().stream().anyMatch(leader -> leader.getFaction().equals(Faction.REBELLION))) {
            showMessage(
                    "The planet has protecting leaders",
                    "Planet can not be searched while it has protecting leaders."
            );
            return;
        }

        if (selectedPlanet.getName().equals(GameState.getSecretBaseLocationStatic().getName())) {
            showGameOver(Faction.EMPIRE);
        }
        else {
            showMessage("No secret base", "You must continue searching.");
            GameUtils.nextTurn();
        }
    }

    public void showMessage(String title, String message) {
        lblMsgTitle.setText(title);
        lblMsgContent.setText(message);
        messagePane.setVisible(true);
        LogUtils.logInfo("Message pane shown: " + title + ", " + message);
    }

    public void showGameOver(Faction winner) {
        if (Faction.REBELLION.equals(winner)) {
            lblWhoWon.setText("The rebellion won!");
        }
        else {
            lblWhoWon.setText("The empire found the secret base and won!");
        }
        gameOverPane.setVisible(true);
    }

    @FXML
    public void attackPlanet() {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);

        if (selectedPlanet != null && !GameUtils.hasEmpireLeader(selectedPlanet)) {
            showMessage("No leaders", "The planet has no empire leaders to carry out the attack");
            return;
        }

        if (selectedPlanet != null && !selectedPlanet.getLeaders().isEmpty()) {
            var removedLeader = initiateCombat(selectedPlanet);
            removeLeaderFromPlanet(removedLeader);

            if (removedLeader.getFaction().equals(Faction.REBELLION)) {
                showMessage("Mission success!", "The empire captured the rebel leader.");
            }
            else {
                showMessage("Mission fail!", "The empire lost this combat mission.");
            }

            GameUtils.nextTurn();
            NetworkUtils.sendRequestPlayerTwo(GameState.getGameState());
        }
    }

    public Leader initiateCombat(Planet planet) {
        GameState.setSearchingPlanetStatic(planet);
        Leader removedLeader;
        if (Faction.REBELLION.equals(CombatUtils.doCombat(planet.getLeaders()))) {
            GameUtils.reputationDown();
            removedLeader = GameUtils.captureLeader(planet, Faction.EMPIRE);
        }
        else {
            removedLeader = GameUtils.captureLeader(planet, Faction.REBELLION);
        }
        return removedLeader;
    }

    public static void disableControls(boolean b) {
        buttons.forEach(btn -> btn.setDisable(b));
        LogUtils.logInfo("Buttons setDisable: " + b);
    }

    @FXML
    public void finishTurn() {
        SoundUtils.playSound(SoundUtils.READY_SOUND);
        GameUtils.nextTurn();

        if (Boolean.TRUE.equals(GameUtils.gameOver())) {
            gameOverPane.setVisible(true);
        }

        GameUtils.sendUniversalRequest(GameState.getGameState());
    }

    @FXML
    public void exitGame() {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);
        Platform.exit();
    }

    @FXML
    public void viewEvents() throws IOException {
        SceneUtils.launchScene("Events replay", SceneUtils.GAME_REPLAY_WINDOW_NAME, 600, 400);
    }
}