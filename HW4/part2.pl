
% knowledge base

flight(istanbul,izmir).
flight(istanbul,antalya).
flight(istanbul,gaziantep).
flight(istanbul,ankara).
flight(istanbul,van).
flight(istanbul,rize).

flight(burdur,ýsparta).
flight(ýsparta,burdur).

flight(ýsparta,izmir).
flight(izmir,ýsparta).

flight(izmir,istanbul).

flight(antalya,istanbul).
flight(antalya,konya).
flight(antalya,gaziantep).

flight(gaziantep,istanbul).
flight(gaziantep,antalya).

flight(konya,antalya).
flight(konya,ankara).

flight(ankara,konya).
flight(ankara,istanbul).
flight(ankara,van).

flight(van,ankara).
flight(van,istanbul).
flight(van,rize).

flight(rize,van).
flight(rize,istanbuls).

flight(edirne,edremit).
flight(edremit,edirne).

flight(edremit,erzincan).
flight(erzincan,edremit).


distance(istanbul,izmir,329).
distance(izmir,istanbul,329).

distance(istanbul,antalya,483).
distance(antalya,istanbul,483).

distance(istanbul,gaziantep,847).
distance(gaziantep,istanbul,847).

distance(istanbul,ankara,352).
distance(ankara,istanbul,352).

distance(istanbul,van,1262).
distance(van,istanbul,1262).

distance(istanbul,rize,968).
distance(rize,istanbul,968).


distance(burdur,ýsparta,25).
distance(ýsparta,burdur,25).

distance(ýsparta,izmir,309).
distance(izmir,ýsparta,309).

distance(antalya,konya,192).
distance(konya,antalya,192).

distance(antalya,gaziantep,592).
distance(gaziantep,antalya,592).

distance(konya,ankara,227).
distance(ankara,konya,227).

distance(ankara,van,920).
distance(van,ankara,920).

distance(van,rize,373).
distance(rize,van,373).


distance(edirne,edremit,235).
distance(edremit,edirne,235).

distance(edremit,erzincan,1066).
distance(erzincan,edremit,1066).



sroute(From,To,ShortestDistance) :-
    distance(From,To,ShortestDistance).

sroute(From,To,ShortestDistance) :-
    findall(Distance, findRouteDistance(From,To,Distance), DistanceList), min(DistanceList, ShortestDistance).


findRouteDistance(From,To,Distance):- possibleRoute(From,To,[From],Distance).

possibleRoute(From, To, VisitedCities, TotalDistance) :-
    distance(From,Temp,D1), isNotVisited(VisitedCities,Temp),
    (To = Temp, TotalDistance is D1;(possibleRoute(Temp, To, [From | VisitedCities],D2), TotalDistance is D1+D2)).

isNotVisited(VisitedCities,Temp) :- not(member(Temp, VisitedCities)).

% finds minimum at the list
min([First,Second|Z],Min) :-
    (First =< Second, min([First|Z],Min));
    (First > Second, min([Second|Z],Min)).

min([Min],Min).



