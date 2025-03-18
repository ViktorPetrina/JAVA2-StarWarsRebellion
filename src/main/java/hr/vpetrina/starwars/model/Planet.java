package hr.vpetrina.starwars.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@AllArgsConstructor
@Getter
@Setter
public class Planet {
    private String name;
    private ControlStatus controlStatus;
    private List<Leader> leaders;
}

