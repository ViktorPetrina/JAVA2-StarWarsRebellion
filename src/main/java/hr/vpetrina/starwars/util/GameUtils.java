package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.model.Leader;
import javafx.scene.control.Label;
import javafx.scene.image.ImageView;

import java.util.List;

public class GameUtils {
    public static void initializeLeaders(List<Leader> leaders, List<Label> labels, List<ImageView> images) {
        for (int i = 0; i < leaders.size(); i++) {
            labels.get(i).setText(leaders.get(i).getName());
            SceneUtils.setImage(images.get(i), leaders.get(i).getImagePath());
        }
    }
}
