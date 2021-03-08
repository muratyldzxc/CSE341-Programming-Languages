
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
flight(rize,istanbul).

flight(edirne,edremit).
flight(edremit,edirne).

flight(edremit,erzincan).
flight(erzincan,edremit).

%rules..

route(X, Y) :-  possibleRoute(X, Y, [X]).

isNotVisited(VisitedCities,Temp) :- not(member(Temp, VisitedCities)).

possibleRoute(From, To, VisitedCities) :-
    flight(From, Temp), isNotVisited(VisitedCities,Temp),
    (To = Temp; possibleRoute(Temp, To, [Temp | VisitedCities])).




