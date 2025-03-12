package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.model.*;
import hr.vpetrina.starwars.util.GameUtils;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.ImageView;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ChooseLeaderController {

    // 1000, 1200 dimensions

    private static final List<Leader> rebelLeaders = new ArrayList<>(List.of(
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

    private static final List<Leader> empireLeaders = new ArrayList<>(List.of(
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

    @FXML
    private ImageView leaderImage1, leaderImage2, leaderImage3, leaderImage4;
    @FXML
    private Label leaderName1, leaderName2, leaderName3, leaderName4;
    @FXML
    private Button chooseButton1, chooseButton2, chooseButton3, chooseButton4;

    private List<ImageView> leaderImages;
    private List<Label> leaderNames;
    private List<Button> chooseButtons;

    @FXML
    public void initialize() {
        leaderImages = Arrays.asList(leaderImage1, leaderImage2, leaderImage3, leaderImage4);
        leaderNames = Arrays.asList(leaderName1, leaderName2, leaderName3, leaderName4);
        chooseButtons = Arrays.asList(chooseButton1, chooseButton2, chooseButton3, chooseButton4);

        initializeEmpireLeaders();
    }


    private void initializeEmpireLeaders() {
        GameUtils.initializeLeaders(empireLeaders, leaderNames, leaderImages);
    }

    private void initializeRebelLeaders() {
        GameUtils.initializeLeaders(rebelLeaders, leaderNames, leaderImages);
    }
}
