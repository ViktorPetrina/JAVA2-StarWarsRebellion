package hr.vpetrina.starwars.util;

import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Control;
import javafx.stage.Stage;

import java.io.IOException;

public class SceneUtils {

    private static final String PATH = "/hr/vpetrina/starwars/";

    public static final String MAIN_WINDOW_FILE_NAME = "main-board-view.fxml";
    public static final String PICK_SIDE_WINDOW_FILE_NAME = "pick-a-side-view.fxml";
    public static final String CHOOSE_A_LEADER_WINDOW_FILE_NAME = "choose-leaders-view.fxml";
    public static final String GAME_REPLAY_WINDOW_FILE_NAME = "game-replay-view.fxml";

    private SceneUtils() {}

    public static void launchScene(String title, String resourceName, Integer width, Integer height) throws IOException {
        FXMLLoader loader = new FXMLLoader(SceneUtils.class.getResource(PATH + resourceName));
        Parent root = loader.load();
        Stage newStage = new Stage();
        newStage.setScene(new Scene(root, width, height));
        newStage.setTitle(title);
        newStage.show();
        newStage.toFront();
    }

    public static void launchSceneFullscreen(String title, String resourceName) throws IOException {
        FXMLLoader loader = new FXMLLoader(SceneUtils.class.getResource(PATH + resourceName));
        Parent root = loader.load();
        Stage newStage = new Stage();
        newStage.setScene(new Scene(root));
        newStage.setTitle(title);
        newStage.show();
        newStage.toFront();
        newStage.setMaximized(true);
    }

    public static void closeWindow(Control control) {
        control.getScene().getWindow().hide();
    }

    public static void showInformationDialog(String title, String header, String content) {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle(title);
        alert.setHeaderText(header);
        alert.setContentText(content);

        alert.showAndWait();
    }

    public static void showWarningDialog(String title, String header, String content) {
        Alert alert = new Alert(Alert.AlertType.WARNING);
        alert.setTitle(title);
        alert.setHeaderText(header);
        alert.setContentText(content);

        alert.showAndWait();
    }
}
