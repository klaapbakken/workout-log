USE workoutLog;

SELECT 
    *
FROM
    exercisewithoutequipment
        NATURAL JOIN
    exercise
        NATURAL JOIN
    exerciseinworkout
        NATURAL JOIN
    workout;
    
SELECT 
    *
FROM
    workout
        NATURAL JOIN
    exerciseinworkout
        NATURAL JOIN
    exercisewithoutequipment;

SELECT 
    *
FROM
    workout
ORDER BY workoutDatetime DESC
LIMIT 10;

SELECT 
    *
FROM
    workout
WHERE
    workoutDatetime BETWEEN '2013-01-01 00:00:00' AND '2015-01-01 00:00:00';

SELECT 
    groupID, groupDescription
FROM
    exerciseGroup
WHERE
    groupID IN (SELECT 
            groupID
        FROM
            exerciseInGroup
        WHERE
            exerciseID IN (SELECT 
                    exerciseID
                FROM
                    exerciseInGroup
                WHERE
                    exerciseID = (SELECT 
                            exerciseID
                        FROM
                            exercise
                        WHERE
                            exerciseName = 'Push-ups')));