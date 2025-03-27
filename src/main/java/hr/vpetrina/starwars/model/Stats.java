package hr.vpetrina.starwars.model;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.io.Serializable;

@AllArgsConstructor
@Getter
public class Stats implements Serializable {
    private Integer combat;
    private Integer espionage;
}
