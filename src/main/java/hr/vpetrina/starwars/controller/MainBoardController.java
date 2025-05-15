package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.constant.Planets;
import hr.vpetrina.starwars.model.*;
import hr.vpetrina.starwars.rmi.ChatRemoteService;
import hr.vpetrina.starwars.util.*;
import javafx.animation.Timeline;
import javafx.application.Platform;
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
import java.util.List;

public class MainBoardController {

    @FXML
    public Label lblMessage;
    @FXML
    public Label lblLastEvent;

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

    @FXML
    public TextArea chatMessagesTextArea;
    @FXML
    public TextField chatMessageTextField;
    @FXML
    public Button btnFinishTurn;
    @FXML
    public Label lblTurn;

    @FXML
    private Pane menuPane;
    @FXML
    private Pane planetMenuPane;

    @FXML
    private Pane chatPane;

    @FXML
    public Label lblPlanetName;
    @FXML
    public Label lblPlanetLeaders;

    @FXML
    private Pane mainPane;

    protected static final List<Button> buttons = new ArrayList<>();
    public static Label lblTurnStatic;
    public static Label lblWhoWonStatic;
    public static Pane gameOverPaneStatic;

    private List<ImageView> planetImages;
    private static Planet selectedPlanet;
    private Boolean menuOpened = false;
    private Boolean chatOpened = false;

    private ChatRemoteService chatRemoteService;

    @FXML
    public void initialize() {
        initializeChatService();
        initializeStatics();
        initializePlanets();
        initializeTimePositions();
        initializePanes();
        initializeSecretBase();
        initializeEventListeners();
        initializeLastEventTimeline();
        initializeLeaders();
    }

    private void initializeLeaders() {
        GameUtils.initializeLeaders(
                GameState.getPlayerLeadersStatic(),
                List.of(leaderName1, leaderName2),
                List.of(leaderImage1, leaderImage2),
                getStats()
        );
    }

    private void initializeLastEventTimeline() {
        Timeline showTheLastGameMoveTimeline = ThreadUtils.getTheLastEventRefreshTimeline(lblLastEvent);
        showTheLastGameMoveTimeline.play();
    }

    private void initializeChatService() {
        Optional<ChatRemoteService> chatRemoteServiceOptional = ChatUtils.initializeChatRemoteService();
        chatRemoteServiceOptional.ifPresent(remoteService -> {
            chatRemoteService = remoteService;
            Timeline chatMessagesRefreshTimeLine = ChatUtils.getChatRefreshTimeline(chatRemoteService, chatMessagesTextArea);
            chatMessagesRefreshTimeLine.play();
        });
    }

    private void initializeStatics() {
        buttons.add(btnAttackPlanet);
        buttons.add(btnSearchPlanet);
        buttons.add(btnFinishTurn);
        buttons.add(btnLeader1);
        buttons.add(btnLeader2);

        lblTurnStatic = lblTurn;
        lblWhoWonStatic = lblWhoWon;
        gameOverPaneStatic = gameOverPane;
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

        GameUtils.setPlanets(Planets.getPlanets());
    }

    private void initializeEventListeners() {
        if (GameState.getPlayerFactionStatic() == Faction.EMPIRE || Boolean.TRUE.equals(GameUtils.isSecretBaseSelected())) {
            planetImages.forEach(planet -> planet.setOnMouseClicked(_ -> openPlanetMenu(planet)));
        }
        mainPane.requestFocus();
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
                GameUtils.setSecretBase(selectedPlanet, lblMessage);
            }

            initializeEventListeners();
            NetworkUtils.sendRequestPlayerOne(GameState.getGameState());
        }));
    }

    private void initializePanes() {
        menuPane.setVisible(false);
        planetMenuPane.setVisible(false);
        messagePane.setVisible(false);
        gameOverPane.setVisible(false);
        chatPane.setVisible(false);
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

    @FXML
    private void deployLeader1() {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);
        if (GameUtils.addLeaderToPlanet(0, selectedPlanet).equals(DeployLeaderResult.IS_CAPTURED)) {
            showMessage("Leader is captured", "Leader is captured for two turns");
        }

    }

    @FXML
    private void deployLeader2() {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);
        if (GameUtils.addLeaderToPlanet(1, selectedPlanet).equals(DeployLeaderResult.IS_CAPTURED)) {
            showMessage("Leader is captured", "Leader is captured for two turns");
        }
    }

    private void openPlanetMenu(ImageView planetImage) {
        selectPlanet(planetImage);
        lblPlanetName.setText(selectedPlanet.getName());

        if (selectedPlanet.getLeaders().isEmpty()) {
            lblPlanetLeaders.setText("None");
        }
        else {
            StringBuilder sb = new StringBuilder();
            selectedPlanet.getLeaders().forEach(leader -> sb.append(leader.getName()).append('\n'));
            lblPlanetLeaders.setText(sb.toString());
        }

        planetMenuPane.setVisible(true);
        SoundUtils.playSound(SoundUtils.MENU_SOUND);
    }

    @FXML
    private void sendMessage() {
        ChatUtils.sendChatMessage(chatRemoteService, chatMessageTextField);
    }

    private void selectPlanet(ImageView planet) {
        selectedPlanet = GameUtils.getPlanets()
                .stream()
                .filter(p -> p.getName().equals(planet.getUserData().toString()))
                .toList()
                .getFirst();
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
        GameUtils.restoreGameState(FileUtils.loadGameState(), false);
        lblMessage.setText("Secret base location: " + GameState.getSecretBaseLocationStatic().getName());
        lblTurn.setText("Turn: " + GameState.getFactionTurnStatic().name());

        initializeEventListeners();
        initializeLeaders();
    }

    private List<List<Label>> getStats() {
        List<Label> leader1Stats = Arrays.asList(leaderStat11, leaderStat12);
        List<Label> leader2Stats = Arrays.asList(leaderStat21, leaderStat22);
        return Arrays.asList(leader1Stats, leader2Stats);
    }

    @FXML
    public void searchPlanet() {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);
        if (GameState.getPlayerFactionStatic().equals(Faction.REBELLION)) {
            showMessage("Invalid action", "Only empire can search planets.");
            return;
        }

        switch (GameUtils.searchPlanet(selectedPlanet)) {
            case IS_PROTECTED -> {
                showMessage(
                    "The planet has protecting leaders",
                    "Planet can not be searched while it has protecting leaders."
                );
                return;
            }
            case NO_SECRET_BASE -> showMessage("No secret base", "You must continue searching.");
            case HAS_SECRET_BASE -> showGameOver(Faction.EMPIRE);
            case NO_LEADERS -> {
                showMessage("No leaders", "You must deploy a leader to search.");
                return;
            }
        }

        finishTurn();
        GameUtils.sendUniversalRequest(GameState.getGameState());
    }

    @FXML
    public void attackPlanet() {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);

        var result = CombatUtils.attackPlanet(selectedPlanet);

        switch (result.getOutcome()) {
            case NO_LEADERS -> {
                showMessage("No leaders", "The planet has no leaders to carry out the attack");
                return;
            }
            case FAILURE -> showMessage("Empire lost!", "The empire lost this combat mission.");
            case SUCCESS -> {
                showMessage("Empire won!", "The empire captured the rebel leader.");
                GameState.setCapturedLeaderStatic(result.getCapturedLeader());
                GameUtils.setCapturedLeaderTurn(GameUtils.getCurrentTurn());
            }
        }

        finishTurn();
        GameUtils.sendUniversalRequest(GameState.getGameState());
    }

    public void showMessage(String title, String message) {
        lblMsgTitle.setText(title);
        lblMsgContent.setText(message);
        messagePane.setVisible(true);
    }

    public void showGameOver(Faction winner) {
        if (Faction.REBELLION.equals(winner)) {
            lblWhoWon.setText("The rebellion won!");
        }
        else {
            lblWhoWon.setText("The empire found the secret base and won!");
        }
        gameOverPane.setVisible(true);
        GameState.getGameOverStatic().setWinner(winner);
        GameState.getGameOverStatic().setIsOver(true);
        GameUtils.sendUniversalRequest(GameState.getGameState());
    }

    public static void disableControls(boolean b) {
        buttons.forEach(btn -> btn.setDisable(b));
    }

    @FXML
    public void finishTurn() {
        SoundUtils.playSound(SoundUtils.READY_SOUND);
        var result = GameUtils.nextTurn(lblTurn);

        if (result.equals(NextTurnResult.GAME_OVER)){
            showGameOver(Faction.REBELLION);
        }

        disableControls(true);
        GameUtils.sendUniversalRequest(GameState.getGameState());
    }

    @FXML
    public void exitGame() {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);
        Platform.exit();
    }

    @FXML
    public void viewEvents() throws IOException {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);
        SceneUtils.launchScene("Events replay", SceneUtils.GAME_REPLAY_WINDOW_FILE_NAME, 600, 400);
    }
}