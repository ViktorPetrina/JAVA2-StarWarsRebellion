package hr.vpetrina.starwars.model;

import lombok.Getter;
import lombok.Setter;

import java.util.List;

public class GameState {

    private GameState() {}

    @Getter @Setter
    private static Faction playerOneFaction;

    @Getter @Setter
    private static Faction playerTwoFaction;

    @Getter @Setter
    private static List<Leader> playerOneLeaders;

    @Getter @Setter
    private static List<Leader> playerTwoLeaders;
}
