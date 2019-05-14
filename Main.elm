module Main exposing (main, polyhedronEdges, testPlanes)

import Axis3d exposing (Axis3d)
import Direction3d exposing (Direction3d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Interval exposing (Interval)
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Svg exposing (Svg)
import Vector3d exposing (Vector3d)


{-| Will eventually be Plane3d.intersectionAxis in elm-geometry
-}
planeIntersection : Plane3d -> Plane3d -> Maybe Axis3d
planeIntersection firstPlane secondPlane =
    let
        firstPoint =
            Plane3d.originPoint firstPlane

        firstNormal =
            Direction3d.toVector (Plane3d.normalDirection firstPlane)

        secondPoint =
            Plane3d.originPoint secondPlane

        secondNormal =
            Direction3d.toVector (Plane3d.normalDirection secondPlane)

        axisVector =
            Vector3d.crossProduct firstNormal secondNormal
    in
    case Vector3d.direction axisVector of
        Just axisDirection ->
            let
                firstVector =
                    Vector3d.crossProduct axisVector firstNormal

                secondVector =
                    Vector3d.crossProduct axisVector secondNormal

                displacement =
                    Vector3d.from firstPoint secondPoint

                d1 =
                    Vector3d.dotProduct displacement secondNormal
                        / Vector3d.dotProduct firstVector secondNormal

                d2 =
                    -(Vector3d.dotProduct displacement firstNormal
                        / Vector3d.dotProduct secondVector firstNormal
                     )

                axisOrigin =
                    if abs d1 <= abs d2 then
                        firstPoint |> Point3d.translateBy (firstVector |> Vector3d.scaleBy d1)

                    else
                        secondPoint |> Point3d.translateBy (secondVector |> Vector3d.scaleBy d2)
            in
            Just (Axis3d.through axisOrigin axisDirection)

        Nothing ->
            Nothing


{-| Will eventually be Axis3d.intersectionWithPlane in elm-geometry
-}
axisPlaneIntersection : Axis3d -> Plane3d -> Maybe Point3d
axisPlaneIntersection givenAxis givenPlane =
    let
        axisDirection =
            Axis3d.direction givenAxis

        normalDirection =
            Plane3d.normalDirection givenPlane

        normalComponent =
            axisDirection |> Direction3d.componentIn normalDirection
    in
    if normalComponent == 0 then
        Nothing

    else
        let
            perpendicularDistance =
                Point3d.signedDistanceFrom givenPlane (Axis3d.originPoint givenAxis)
        in
        Just (Point3d.along givenAxis -(perpendicularDistance / normalComponent))


{-| Given a set of trim planes (with normals oriented outwards), return a list of edges of the
resulting polyhedron. Only finite-length edges will be returned.
-}
polyhedronEdges : List Plane3d -> List LineSegment3d
polyhedronEdges givenPlanes =
    case givenPlanes of
        first :: rest ->
            polyhedronEdgesHelp first rest givenPlanes []

        [] ->
            []


{-| Loop through each plane and test it against all following planes
-}
polyhedronEdgesHelp : Plane3d -> List Plane3d -> List Plane3d -> List LineSegment3d -> List LineSegment3d
polyhedronEdgesHelp currentPlane followingPlanes allPlanes accumulatedEdges =
    case followingPlanes of
        next :: rest ->
            let
                updatedEdges =
                    List.foldl
                        (findIntersectionEdge allPlanes currentPlane)
                        accumulatedEdges
                        followingPlanes
            in
            polyhedronEdgesHelp next rest allPlanes updatedEdges

        [] ->
            accumulatedEdges


{-| Find intersection edge between two planes (trimmed to all other planes), and add it to the list
of accumulated line segments
-}
findIntersectionEdge : List Plane3d -> Plane3d -> Plane3d -> List LineSegment3d -> List LineSegment3d
findIntersectionEdge allPlanes firstPlane secondPlane accumulatedEdges =
    case planeIntersection firstPlane secondPlane of
        Just intersectionAxis ->
            let
                initialInterval =
                    Just (Interval.from (-1 / 0) (1 / 0))

                intersectionInterval =
                    List.foldl
                        (trimIntersection firstPlane secondPlane intersectionAxis)
                        initialInterval
                        allPlanes
            in
            case intersectionInterval of
                Nothing ->
                    -- No intersection edge, just return the ones accumulated so far
                    accumulatedEdges

                Just interval ->
                    if isInfinite (Interval.width interval) then
                        -- Ignore infinite (unbounded) edges
                        accumulatedEdges

                    else
                        let
                            ( startDistance, endDistance ) =
                                Interval.endpoints interval
                        in
                        -- Add the newly found line segmented to the accumulated list
                        LineSegment3d.along intersectionAxis startDistance endDistance
                            :: accumulatedEdges

        Nothing ->
            accumulatedEdges


{-| Trim a given line segment (defined by a possible interval of distance along an axis) to a given
plane
-}
trimIntersection : Plane3d -> Plane3d -> Axis3d -> Plane3d -> Maybe Interval -> Maybe Interval
trimIntersection firstPlane secondPlane axis trimPlane currentInterval =
    case currentInterval of
        Just interval ->
            -- Don't trim to the two planes that intersected to form this axis
            if trimPlane == firstPlane || trimPlane == secondPlane then
                currentInterval

            else
                -- See if the axis intersects the trim plane
                case axisPlaneIntersection axis trimPlane of
                    Just intersectionPoint ->
                        let
                            distanceAlongAxis =
                                intersectionPoint |> Point3d.signedDistanceAlong axis

                            normalDirection =
                                Plane3d.normalDirection trimPlane

                            axisDirection =
                                Axis3d.direction axis

                            normalComponent =
                                axisDirection |> Direction3d.componentIn normalDirection

                            -- Check whether we should trim axial distance values above or below
                            -- the intersection point, based on the relative orientation of the
                            -- axis and plane
                            trimInterval =
                                if normalComponent > 0 then
                                    -- Plane is max bound
                                    Interval.from (-1 / 0) distanceAlongAxis

                                else
                                    -- Plane is min bound
                                    Interval.from distanceAlongAxis (1 / 0)
                        in
                        Interval.intersection interval trimInterval

                    Nothing ->
                        if Point3d.signedDistanceFrom trimPlane (Axis3d.originPoint axis) > 0 then
                            -- Entire axis is on outside of trim plane
                            Nothing

                        else
                            -- Entire axis is on inside of trim plane
                            currentInterval

        Nothing ->
            -- Entire axis has already been eliminated
            Nothing


{-| Convenience function to make a plane based on the coordinates of its origin plus the azimuth
and elevation angles of its normal direction
-}
plane : ( Float, Float, Float ) -> ( Float, Float ) -> Plane3d
plane originCoordinates ( normalAzimuth, normalElevation ) =
    Plane3d.through (Point3d.fromCoordinates originCoordinates)
        (Direction3d.fromAzimuthAndElevation normalAzimuth normalElevation)


{-| List of planes defining a trapezoidal block shape
-}
testPlanes : List Plane3d
testPlanes =
    [ plane ( 0, 0, 0 ) ( degrees 180, degrees 45 ) -- back
    , plane ( 5, 2, 0 ) ( degrees 0, degrees 45 ) -- front
    , plane ( 0, 2, 1 ) ( degrees 0, degrees -90 ) -- bottom
    , plane ( 0, 2, 2 ) ( degrees 0, degrees 90 ) -- top
    , plane ( 0, 0, 0 ) ( degrees -90, degrees 0 ) -- right
    , plane ( 1, 3, 5 ) ( degrees 90, degrees 0 ) -- left
    ]


{-| Show the edges as text (proper visualization is left as an exercise!)
-}
main : Html msg
main =
    Html.div []
        (polyhedronEdges testPlanes
            |> List.map (\edge -> Html.div [] [ Html.text (Debug.toString edge) ])
        )
