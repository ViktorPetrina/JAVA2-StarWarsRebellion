package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.model.*;
import hr.vpetrina.starwars.util.GameUtils;
import hr.vpetrina.starwars.util.SceneUtils;
import hr.vpetrina.starwars.util.SoundUtils;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.ImageView;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ChooseLeaderController {

    // 1000, 1200 dimensions

    private static final List<Leader> rebelLeaders = GameUtils.getRebelLeaders();
    private static final List<Leader> empireLeaders = GameUtils.getEmpireLeaders();

    @FXML
    public Label lblLeaders;

    @FXML
    public Button btnReady;
    @FXML
    public Button btnReset;

    @FXML
    public Label leaderStat11;
    @FXML
    public Label leaderStat12;

    @FXML
    public Label leaderStat21;
    @FXML
    public Label leaderStat22;

    @FXML
    public Label leaderStat31;
    @FXML
    public Label leaderStat32;

    @FXML
    public Label leaderStat41;
    @FXML
    public Label leaderStat42;

    @FXML
    private ImageView leaderImage1;
    @FXML
    private ImageView leaderImage2;
    @FXML
    private ImageView leaderImage3;
    @FXML
    private ImageView leaderImage4;

    @FXML
    private Label leaderName1;
    @FXML
    private Label leaderName2;
    @FXML
    private Label leaderName3;
    @FXML
    private Label leaderName4;

    @FXML
    private Button chooseButton1;
    @FXML
    private Button chooseButton2;
    @FXML
    private Button chooseButton3;
    @FXML
    private Button chooseButton4;

    private List<Button> chooseButtons;

    private final List<Leader> selectedLeaders = new ArrayList<>();

    @FXML
    public void initialize() {
        List<Label> leaderNames = Arrays.asList(leaderName1, leaderName2, leaderName3, leaderName4);
        List<ImageView> leaderImages = Arrays.asList(leaderImage1, leaderImage2, leaderImage3, leaderImage4);
        List<List<Label>> stats = getStats();

        chooseButtons = Arrays.asList(chooseButton1, chooseButton2, chooseButton3, chooseButton4);

        if (GameState.getPlayerFactionStatic() == Faction.EMPIRE) {
            GameUtils.initializeLeaders(empireLeaders, leaderNames, leaderImages, stats);
            addButtonEventListeners(empireLeaders);
        }
        else {
            GameUtils.initializeLeaders(rebelLeaders, leaderNames, leaderImages, stats);
            addButtonEventListeners(rebelLeaders);
        }
    }

    private List<List<Label>> getStats() {
        List<Label> leader1Stats = Arrays.asList(leaderStat11, leaderStat12);
        List<Label> leader2Stats = Arrays.asList(leaderStat21, leaderStat22);
        List<Label> leader3Stats = Arrays.asList(leaderStat31, leaderStat32);
        List<Label> leader4Stats = Arrays.asList(leaderStat41, leaderStat42);
        return Arrays.asList(leader1Stats, leader2Stats, leader3Stats, leader4Stats);
    }

    private void addButtonEventListeners(List<Leader> leaders) {
        for (int i = 0; i < chooseButtons.size(); i++) {
            int index = i;
            chooseButtons.get(i).setOnAction(_ -> {
                SoundUtils.playSound(SoundUtils.SELECT_SOUND);
                if (selectedLeaders.contains(leaders.get(index))) {
                    SceneUtils.showWarningDialog(
                            GameUtils.GAME_ERROR,
                            "Leader selected",
                            "The leader you choose is already selected"
                    );
                    return;

                } else if (selectedLeaders.size() == 2) {
                    SceneUtils.showWarningDialog(
                            GameUtils.GAME_ERROR,
                            "Too many leaders",
                            "You can select only two leaders"
                    );
                    return;
                }

                selectedLeaders.add(leaders.get(index));
                lblLeaders.setText(leadersToString(selectedLeaders));
            });
        }
    }

    private String leadersToString(List<Leader> leaders) {
        if (leaders.isEmpty()) {
            return "No leaders selected";
        }

        StringBuilder builder = new StringBuilder("Selected: ");
        leaders.forEach(leader -> builder.append(leader.getName()).append(", "));
        return builder.substring(0, builder.length() - 2);
    }

    @FXML
    private void resetChoices() {
        SoundUtils.playSound(SoundUtils.RESET_SOUND);
        selectedLeaders.clear();
        lblLeaders.setText(leadersToString(selectedLeaders));
    }

    @FXML
    public void startGame() throws IOException {
        SoundUtils.playSound(SoundUtils.READY_SOUND);
        if (selectedLeaders.size() < 2) {
            SceneUtils.showWarningDialog(
                    GameUtils.GAME_ERROR,
                    "Not enough leaders selected",
                    "Two leaders must be selected to start the game");

            return;
        }

        GameState.setPlayerLeadersStatic(selectedLeaders);
        SceneUtils.launchSceneFullscreen(GameUtils.TITLE, SceneUtils.MAIN_WINDOW_NAME);
        SceneUtils.closeWindow(lblLeaders);
    }
}
