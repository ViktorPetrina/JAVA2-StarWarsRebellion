package hr.vpetrina.starwars.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class GameMove implements Serializable {
    private Planet planet;
    private List<Leader> leaders = new ArrayList<>();
    private MoveType moveType;
    private Faction winner;
    private Faction executor;

    public GameMove(Planet planet, MoveType moveType, Faction winner, Faction executor) {
        this.planet = planet;
        this.moveType = moveType;
        this.winner = winner;
        this.executor = executor;
    }

    public GameMove(Planet planet, MoveType moveType, Faction executor) {
        this.planet = planet;
        this.moveType = moveType;
        this.executor = executor;
    }
}
