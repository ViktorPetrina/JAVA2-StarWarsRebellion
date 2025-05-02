package hr.vpetrina.starwars.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;

@AllArgsConstructor
@Getter
@Setter
public class Planet implements Serializable {
    private String name;
    private ControlStatus controlStatus;
    private List<Leader> leaders;

    public Planet(String name) {
        this.name = name;
    }
}

