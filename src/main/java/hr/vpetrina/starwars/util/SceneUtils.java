package hr.vpetrina.starwars.util;

import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;

public class SceneUtils {

    private static final String PATH = "/hr/vpetrina/starwars/";

    public static void launch(String title, String resourceName, Integer width, Integer height) throws IOException {
        FXMLLoader loader = new FXMLLoader(SceneUtils.class.getResource(PATH + resourceName));
        Parent root = loader.load();
        Stage newStage = new Stage();
        newStage.setScene(new Scene(root, width, height));
        newStage.setTitle(title);
        newStage.show();
    }
}
