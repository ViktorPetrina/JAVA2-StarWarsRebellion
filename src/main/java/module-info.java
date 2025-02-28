module hr.vpetrina.starwars.rebellion {
    requires javafx.controls;
    requires javafx.fxml;


    opens hr.vpetrina.starwars.rebellion to javafx.fxml;
    exports hr.vpetrina.starwars.rebellion;
}