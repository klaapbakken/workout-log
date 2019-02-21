USE oyvinkla_workoutLog;

CREATE TABLE workout (
    workoutId INT NOT NULL AUTO_INCREMENT,
    workoutDatetime DATETIME NOT NULL,
    workoutLength INT,
    shape INT,
    performance INT,
    note VARCHAR(2048),
    PRIMARY KEY (workoutId)
);

CREATE TABLE exercise (
    exerciseId INT NOT NULL AUTO_INCREMENT,
    exerciseName VARCHAR(64) NOT NULL,
    PRIMARY KEY (exerciseId)
);

CREATE TABLE equipment (
    equipmentId INT NOT NULL AUTO_INCREMENT,
    equipmentName VARCHAR(128),
    description VARCHAR(1024),
    PRIMARY KEY (equipmentId)
);

CREATE TABLE exerciseInWorkout (
    workoutId INT NOT NULL,
    exerciseId INT NOT NULL,
    PRIMARY KEY (workoutId , exerciseId),
    FOREIGN KEY (workoutId)
        REFERENCES workout (workoutId),
    FOREIGN KEY (exerciseId)
        REFERENCES exercise (exerciseId)
);

CREATE TABLE exerciseWithEquipment (
    exerciseWithEquipmentId INT NOT NULL AUTO_INCREMENT,
    exerciseId INT NOT NULL,
    equipmentId INT NOT NULL,
    kg DECIMAL,
    sets INT,
    PRIMARY KEY (exerciseWithEquipmentId),
    FOREIGN KEY (exerciseId)
        REFERENCES exercise (exerciseId),
    FOREIGN KEY (equipmentId)
        REFERENCES equipment (equipmentId)
);

CREATE TABLE exerciseWithoutEquipment (
    exerciseWithoutEquipmentId INT NOT NULL AUTO_INCREMENT,
    exerciseId INT NOT NULL,
    description VARCHAR(1024),
    PRIMARY KEY (exerciseWithoutEquipmentId),
    FOREIGN KEY (exerciseId)
        REFERENCES exercise (exerciseId)
);

CREATE TABLE exerciseGroup (
    groupID INT NOT NULL AUTO_INCREMENT,
    groupDescription VARCHAR(1024),
    PRIMARY KEY (groupID)
);
    
CREATE TABLE exerciseInGroup (
    groupID INT NOT NULL,
    exerciseID INT NOT NULL,
    PRIMARY KEY (groupID , exerciseID),
    FOREIGN KEY (exerciseID)
        REFERENCES exercise (exerciseID),
    FOREIGN KEY (groupID)
        REFERENCES exerciseGroup (groupID)
);
    