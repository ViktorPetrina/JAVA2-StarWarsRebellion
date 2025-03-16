package hr.vpetrina.starwars.model;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public class Stats {
    private Integer diplomacy;
    private Integer tactics;
    private Integer espionage;
    private Integer logistics;
}
