package hr.vpetrina.starwars.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@AllArgsConstructor
public class GameState {

    private Planet secretBaseLocation;

    @Getter @Setter
    private static Faction playerFaction;

    @Getter @Setter
    private static List<Leader> playerLeaders;

    @Getter @Setter
    private static Player currentPlayer;
}
