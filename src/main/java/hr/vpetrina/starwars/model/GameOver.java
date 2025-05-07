package hr.vpetrina.starwars.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
@AllArgsConstructor
public class GameOver implements Serializable {
    private Boolean isOver;
    private Faction winner;
}
