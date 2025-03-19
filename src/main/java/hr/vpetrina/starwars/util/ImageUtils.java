package hr.vpetrina.starwars.util;

import javafx.scene.image.Image;
import javafx.scene.image.ImageView;

import java.util.Objects;

public class ImageUtils {

    public static final String TIME_TRACKER_IMAGE = "/hr/vpetrina/starwars/images/time_marker.png";
    public static final String REPUTATION_TRACKER_IMAGE = "/hr/vpetrina/starwars/images/rebellion_marker.png";

    private ImageUtils() {}

    public static void setImage(ImageView imageView, String imagePath) {
        imageView.setImage(new Image(Objects.requireNonNull(SceneUtils.class.getResource(imagePath)).toExternalForm()));
    }
}
