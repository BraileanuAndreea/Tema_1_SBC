
%Fapte%
oua(5). %bucati
unt(200). %g
zahar(300). %g
faina(700). %g
rom(100). %ml
cafea(300). %ml
mascarpone(300). %g
frisca(300). %g
piscoturi(2). %pachete
mere(5). %bucati
migdale(50). %g
gem_caise(100). %g
lamaie(2).
suc_de_mere(1). %l
faina_de_migdale(100). %g
margarina(300). %g
biscuiti(300). %g
ciocolata_alba(300). %g
foi_napolitana(10).
nuca_macinata(300). %g
lapte(2000). %ml
miere(100). %ml
apa(1000). %ml
ulei(1000). %ml
amoniac(3). %pliculete
otet(1000). %ml
nuca_de_cocos(3). %pliculete
zahar_vanilat(5). %pliculete
vanilie().
cacao().
biscuiti(500). %g
ciocolata_alba(500). %g
sirop_de_capsune(100). %ml
nuca_de_cocos(100). %g
ciocolata_neagra(300). %g
ciocolata_alba(300). %g
praf_de_copt(200). %g

main:- intro,
    resetare_raspunsuri,
    gaseste_prajitura(Prajitura),
    mod_de_preparare(Prajitura).

:- dynamic(progres/2).

intro:-
  write('Ce prajitura vreti sa faceti astazi?'), nl,
  write('Pentru a raspunde, tastati numarul corespunzator raspunsului, urmat de punct (.)'), nl, nl.

resetare_raspunsuri :-
  retract(progres(_, _)),
  fail.
resetare_raspunsuri.

gaseste_prajitura(Prajitura):- prajitura(Prajitura), !.

prajitura(creola):-
    fructe(fara_fructe),
    vanilie(fara_vanilie),
    frisca(fara_frisca),
    cafea(cu_cafea),
    rom(cu_rom),
    unt(Q), Q >= 200, zahar(W), W >= 200, miere(E), E >= 20, oua(R), R >= 4, faina(T), T >= 250, cafea(Y),
    Y >= 100, rom(U), U >= 30, ciocolata_neagra(I), I >= 100.

prajitura(fursecuri_cu_vanilie):-
    fructe(fara_fructe),
    vanilie(cu_vanilie),
    frisca(fara_frisca),
    cafea(fara_cafea),
    rom(fara_rom),
    oua(X), X >= 1, unt(Y), Y >= 200, faina(Z), Z >= 300, vanilie().

prajitura(tiramisu):-
    fructe(fara_fructe),
    vanilie(fara_vanilie),
    frisca(cu_frisca),
    cafea(cu_cafea),
    rom(cu_rom),
    piscoturi(X), X >= 2, oua(Y), Y >= 5, zahar(Z), Z >= 150, mascarpone(W), W >= 250, frisca(Q), Q >= 200,
    cafea(E), E >= 200, zahar_vanilat(R), R >= 1, rom(U), U >= 10, cacao().

prajitura(prajitura_cu_mere):-
    fructe(cu_fructe),
    vanilie(fara_vanilie),
    frisca(fara_frisca),
    cafea(fara_cafea),
    rom(fara_rom),
    faina(Q), Q >= 225, unt(W), W >= 125, zahar(E), E >= 125, lamaie(R), R >= 1, oua(T), T >= 1,
    suc_de_mere(Y), Y >= 1, faina_de_migdale(U), U >= 80, migdale(I), I >= 45, mere(A), A >= 4.

prajitura(prajitura_cu_foi_de_napolitana):-
    fructe(fara_fructe),
    vanilie(fara_vanilie),
    frisca(fara_frisca),
    cafea(fara_cafea),
    rom(cu_rom),
    foi_napolitana(Q), Q >= 2, unt(W), W >= 200, biscuiti(E), E >= 300, nuca_macinata(R),
    R >= 250, zahar(T), T >= 250, lapte(Y), Y >= 250, rom(U), U >= 10, ciocolata_alba(I), I >= 300, sirop_de_capsune(O), O >= 20, cacao().

prajitura(tavalita):-
    fructe(fara_fructe),
    vanilie(fara_vanilie),
    frisca(fara_frisca),
    cafea(fara_cafea),
    rom(cu_rom),
    zahar(Q), Q >= 300, ulei(W), W >= 50, miere(E), E >= 10,  amoniac(R), R >= 1, otet(T), T >= 10, apa(Y),
    Y >= 250, faina(U), U >= 500, zahar_vanilat(I), I >= 1, lamaie(O), O >= 1, nuca_de_cocos(P), P >= 20, rom(A), A >= 10, cacao().


prajitura(negresa):-
    fructe(fara_fructe),
    vanilie(fara_vanilie),
    frisca(fara_frisca),
    cafea(fara_cafea),
    rom(fara_rom),
    ciocolata_neagra(Q), Q >= 200, oua(W), W >= 4, praf_de_copt(E), E >= 50.

prajitura(cheesecake_new_york):-
    fructe(fara_fructe),
    vanilie(cu_vanilie),
    frisca(cu_frisca),
    cafea(fara_cafea),
    rom(fara_rom),
    biscuiti(Q), Q >= 200, unt(W), W >= 100, zahar_vanilat(E), E >= 1, vanilie(), frisca(R), R >= 200.

prajitura(prajitura_inteligenta_cu_lamaie):-
    fructe(fara_fructe),
    vanilie(cu_vanilie),
    frisca(fara_frisca),
    cafea(fara_cafea),
    rom(fara_rom),
    oua(Q), Q >= 4, vanilie(), faina(W), W >= 115, lapte(E), E >= 150, lamaie(R), R >= 1.

intrebare(fructe) :-
  write('Doriti sa fie cu fructe?'), nl.

intrebare(vanilie) :-
  write('Doriti sa fie cu vanilie?'), nl.

intrebare(frisca) :-
  write('Doriti sa fie cu frisca?'), nl.

intrebare(cafea) :-
  write('Doriti sa fie cu cafea?'), nl.

intrebare(rom) :-
  write('Doriti sa fie cu rom?'), nl.


raspuns(cu_fructe):-
    write('Da.').
raspuns(fara_fructe):-
    write('Nu.').

raspuns(cu_vanilie):-
    write('Da.').
raspuns(fara_vanilie):-
    write('Nu.').

raspuns(cu_frisca):-
    write('Da.').
raspuns(fara_frisca):-
    write('Nu.').

raspuns(cu_cafea):-
    write('Da.').
raspuns(fara_cafea):-
    write('Nu.').

raspuns(cu_rom):-
    write('Da.').
raspuns(fara_rom):-
    write('Nu.').



fructe(Raspuns) :-
  progres(fructe, Raspuns).
fructe(Raspuns) :-
  \+ progres(fructe, _),
  intreaba(fructe, Raspuns, [fara_fructe, cu_fructe]).

vanilie(Raspuns) :-
  progres(vanilie, Raspuns).
vanilie(Raspuns) :-
  \+ progres(vanilie, _),
  intreaba(vanilie, Raspuns, [fara_vanilie, cu_vanilie]).

frisca(Raspuns) :-
  progres(frisca, Raspuns).
frisca(Raspuns) :-
  \+ progres(frisca, _),
  intreaba(frisca, Raspuns, [fara_frisca, cu_frisca]).

cafea(Raspuns) :-
  progres(cafea, Raspuns).
cafea(Raspuns) :-
  \+ progres(cafea, _),
  intreaba(cafea, Raspuns, [fara_cafea, cu_cafea]).

rom(Raspuns) :-
  progres(rom, Raspuns).
rom(Raspuns) :-
  \+ progres(rom, _),
  intreaba(rom, Raspuns, [fara_rom, cu_rom]).



mod_de_preparare(fursecuri_cu_vanilie):-
    nl,
    write('Fursecuri cu vanilie - MOD DE PREPARARE:'), nl, nl,
    write('1. Preincalzeste cuptorul la 200°.'), nl,
    write('2. Amesteca toate ingredientele pana cand obtii un amestec omogen.'), nl,
    write('3. Formeaza biscuitii danezi cu vanilie formand bilute cu palmele umezite si aplatizandu-le sau, ideal, cu o presa de biscuiti.'), nl,
    write('4. Coace fursecurile daneze timp de 7-8 minute, pana cand devin usor aurii pe partea de jos.'), nl, nl,
    write('-----------------------------------------------------------------------------------------------------------------------------------'), nl, nl.

mod_de_preparare(tiramisu):-
    nl,
    write('Tiramisu - MOD DE PREPARARE:'), nl, nl,
    write('1. Bate galbenusurile cu zaharul pana devin spuma.'), nl,
    write('2. Separat, bate albusurile spuma, apoi incorporeaza frisca si continua sa mixezi.'), nl,
    write('3. Galbenusurile se amesteca cu mascarpone, se adauga compozitia de albusuri cu frisca, urmata de esenta si zaharul vanilat.'), nl,
    write('4. Da piscoturile prin cafeaua usor racita, intinde in tava un rand de piscoturi si peste pune crema obtinuta.'), nl,
    write('Continua sa adaugi randuri de piscoturi alternate de crema, iar la suprafata trebuie sa ramana un ultim strat de crema.'), nl,
    write('Pudreaza tiramisu cu cacao. Da prajitura la frigider timp de 2-3 ore, apoi serveste-o.'), nl.

mod_de_preparare(prajitura_cu_mere):-
    nl,
    write('Prajitura de mere - MOD DE PREPARARE:'), nl, nl,
    write('1. Intr-un vas, se pun faina si untul, zaharul, coaja de lamaie si galbenusul. Se amesteca si apoi se toarna sucul de mere. Se amesteca pentru a obtine un aluat omogen. Se presara faina pe masa de lucru si se intinde  aluatul in functie de dimensiunea tavii de copt.'), nl,
    write('2. Se transfera in tava de copt avand grija ca aluatul sa urce pe peretii vasului. Se da la frigider. Se curata merele si se taie in patru si apoi fiecare sfert se feliaza fara a desprinde complet. Trebuie sa arate ca un evantai.'), nl,
    write('3. Pentru crema de migdale se preseaza de suc jumatate de lamaie, care se bate usor cu oul si galbenusul. Migdalele feliate se maruntesc cu un sucitor. Se amesteca untul moale cu zaharul pentru a obtine o crema spumoasa in care se adauga ouale batute, migdalele usor faramitate si cele doua tipuri de faina. Se intinde crema in forma de copt, peste aluat, egalizand-o.'), nl,
    write('4. Se asaza merele deasupra cremei cu migdale. Prajitura se da la cuptor pentru o ora. Imediat cum se scoate din cuptor, se unge cu gem incalzit.'), nl.

mod_de_preparare(prajitura_cu_foi_de_napolitana):-
    nl,
    write('Prajitura cu foi de napolitana - MOD DE PREPARARE:'), nl, nl,
    write('1. Se fierbe laptele cu zaharul pana clocoteste, apoi se ia de pe foc si se adauga in el untul taiat bucatele, nuca macinata si cele 5 linguri cu cacao. Se adauga apoi in crema biscuitii sfaramati si esenta de rom.
'), nl,
    write('2. Crema se intinde intre foile de napolitana. Prajitura se glazureaza cu ciocolata alba topita la bain-marie si se orneaza cu siropul de capsune.'), nl.

mod_de_preparare(tavalita):-
    nl,
    write('Prajitura tavalita - MOD DE PREPARARE:'), nl, nl,
    write('1. Se pune o cantitate mica de faina intr-un vas si se amesteca impreuna cu amoniacul stins cu otet. Se inglobeaza zaharul si zaharul vanilat, uleiul, mierea, apa si coaja de lamaie si se amesteca bine. La final, se adauga faina, amestecand, pana ce compoziția capata consistenţa unei smantani groase.'), nl,
    write('2. Aluatul obținut se toarna in tava tapetata cu hartie de copt si se da la cuptor pentru 35-40 de minute, pana trece testul cu scobitoarea. Se scoate blatul si se lasa la racit.'), nl,
    write('3. Intre timp, se prepara glazura de ciocolata: se pun pe foc, mai intai, apa, uleiul si zaharul. Cand au dat in clocot, se adauga cacaua si romul si se amesteca bine, astfel incat sa nu existe cocoloase de cacao. Se da glazura de o parte si se lasa sa se raceasca.'), nl,
    write('4. Blatul racorit se taie cuburi potrivite ca marime care se trec pe rand prin glazura, apoi se tavalesc prin nuca de cocos.'), nl.

mod_de_preparare(creola):-
    nl,
    write('Prajitura creola - MOD DE PREPARARE:'), nl.
mod_de_preparare(negresa):-
    nl,
    write('Prajitura creola - MOD DE PREPARARE:'), nl.
mod_de_preparare(cheesecake_new_york):-
    nl,
    write('Prajitura cheesecake New York - MOD DE PREPARARE:'), nl.
mod_de_preparare(prajitura_inteligenta_cu_lamaie):-
    nl,
    write('Prajitura inteligenta_cu_lamaie - MOD DE PREPARARE:'), nl.

raspunsuri([], _).
raspunsuri([First|Rest], Index) :-
  write(Index), write(' '), raspuns(First), nl,
  NextIndex is Index + 1,
  raspunsuri(Rest, NextIndex).


% Parses an Index and returns a Response representing the "Indexth" element in
% Choices (the [First|Rest] list)
parse(0, [First|_], First).
parse(Index, [First|Rest], Rezultat) :-
  Index > 0,
  NextIndex is Index - 1,
  parse(NextIndex, Rest, Rezultat).


% Asks the Question to the user and saves the Answer
intreaba(Intrebare, Raspuns, Optiuni) :-
  intrebare(Intrebare),
  raspunsuri(Optiuni, 0),
  read(Index),
  parse(Index, Optiuni, Rezultat),
  asserta(progres(Intrebare, Rezultat)),
  Rezultat = Raspuns.


