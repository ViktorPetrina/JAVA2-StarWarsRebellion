package hr.vpetrina.starwars.model;

import lombok.Getter;

@Getter
public enum GameMoveTag {
    GAME_MOVE("GameMove"),
    PLANET("Planet"),
    LEADERS("Leaders"),
    LEADER("Leader"),
    MOVE_TYPE("MoveType"),
    WINNER("Winner"),
    EXECUTOR("Executor");

    private final String tagName;

    GameMoveTag(String tagName) {
        this.tagName = tagName;
    }
}


