package hr.vpetrina.starwars.util;

import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.stage.Stage;
import jdk.jshell.spi.ExecutionControl;

import java.io.IOException;
import java.util.Objects;

public class SceneUtils {

    private static final String PATH = "/hr/vpetrina/starwars/";

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

    public static void setImage(ImageView imageView, String imagePath) {
        imageView.setImage(new Image(Objects.requireNonNull(SceneUtils.class.getResource(imagePath)).toExternalForm()));
    }
}
