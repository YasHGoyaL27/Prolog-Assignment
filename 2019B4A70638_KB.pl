/*
NAME : YASH GOYAL
ID : 2019B4A70638P
NAME : AYUSH AGARWAL
ID : 2019B4A70652P
NAME : ANUSHKA JAIN
ID : 2019B1A70904P
NAME : PAYAL BASRANI
ID : 2019B5A70809P
*/


age(rohan, 23).
citizen(rohan, 23).
age(meera, 30).
citizen(meera, 8).
age(david, 35).
citizen(david, 35).
age(leonard, 40).
citizen(leonard, 40).
age(amy, 38).
citizen(amy, 5).
stateOfUS(newHampshire).
stateOfUS(massachusetts).
stateOfUS(connecticut).
stateOfUS(newYork).
stateOfUS(newJersey).
stateOfUS(pennsylvania).
stateOfUS(delaware).
stateOfUS(maryland).
stateOfUS(virginia).
stateOfUS(northCarolina).
stateOfUS(southCarolina).
stateOfUS(georgia).
monday(7, 1, 2019).
monday(2, 12, 2019).

%Article 1

%Section 1

legislativePowers(congress).
member(congress,[senators,houseOfRepresentatives]).

%Section 2

member(houseOfRepresentatives,chosenSecondYearPeople).
qualified(X,houseOfRepresentatives) :- age(X,Y), Y>=25, citizen(X,Z), Z>=7.
numberOfRepresentatives(newHampshire,3).
numberOfRepresentatives(massachusetts,8).
numberOfRepresentatives(connecticut,5).
numberOfRepresentatives(rhodeIslandAndProvidencePlantations,1).
numberOfRepresentatives(newYork,6).
numberOfRepresentatives(newJersey,4).
numberOfRepresentatives(pennsylvania,8).
numberOfRepresentatives(delaware,1).
numberOfRepresentatives(maryland,6).
numberOfRepresentatives(virginia,10).
numberOfRepresentatives(northCarolina,5).
numberOfRepresentatives(southCarolina,5).
numberOfRepresentatives(georgia,3).
power(executiveAuthority,issuesWritsOfElectionToFillRepresentativeVacancies).
power(houseOfRepresentatives,chooseSpeaker).
power(houseOfRepresentatives,impeachmentPower).

%Section3

member(senate,2).
term(senator,6).
vote(senator,1).
classes(senate,3).
qualified(X,senator) :- age(X,Y), Y>=30, citizen(X,Z), Z>=9.
representation(vacancies,fill(election)).  /* change due to amendment 17 */
vicePresidentOfUS(presidentOfSenate).
presidentOfSenate(vicePresidentOfUS).
vote(presidentOfSenate,0).
vote(vicePresidentOfUS,0).
power(senate,chooseOfficers).
power(senate,choosePresidentProTempore).
power(senate,tryImpeachment).
power(chiefJustice,presideWhenPresidentIsTried).

%Section 4


power(legislature,chooseTimePlaceMannerOfElectionForSenatorHouseOfRepresentatives).
power(congress,makeOrAlterRegulations).
meetingOfCongress(3,1,Y).   /* change due to amendment 20 */

%Section 5

power(house,judgeOfElection).
power(house,penaltiesToOtherMembers).
power(house,determine(rules(proceedings))).
power(house,punish(members(disorderlyBehaviours))).
duty(house,journal(proceedings)).

%Section 6

right(senators,receive(compensation)).
right(houseOfRepresentatives,receive(compensation)).
privilege(senators,pardon(arrest(except(treasonFelonyAndBreachOfPease)))).
privilege(houseOfRepresentatives,pardon(arrest(except(treasonFelonyAndBreachOfPease)))).
restriction(senators,not(appointed(civilOffice))).
restriction(houseOfRepresentatives,not(appointed(civilOffice))).

%Section 7

power(houseOfRepresentatives,propose(bills(raising(revenues)))).
power(senate,propose(amendment(bills))).
law(bill(X)) :- presidentOfUS(sign(bill(X))) ; twoThird(bothHouse(pass(bill(X)))). /* test case to be provided X is the name of the bill and it should satisfy either or the two given conditions*/

%Section 8

power(congress,collect(tax)).
power(congress,collect(Duties)).
power(congress,collect(Imports)).
power(congress,collect(Excises)).
power(congress,lay(tax)).
power(congress,lay(Duties)).
power(congress,lay(Imports)).
power(congress,lay(Excises)).
power(congress,pay(debt)).
power(congress,provide(defence)).
power(congress,provide(generalWarfare)).
power(congress,borrow(money)).
power(congress,regulate(commerce)).
power(congress,establish(rule(naturalization))).
power(congress,coin(money)).
power(congress,fix(standards)).
power(congress,punish(counterfeiting(securitiesAndCoin))).
power(congress,establish(postOfficesAndRoads)).
power(congress,promote(scienceAndArts)).
promote(scienceAndArts) :- secure(right(authorsAndInventors)).
power(congress,constitute(inferior(tribunals,supremeCourt))).
power(congress,punish(offenceAgainst(lawOfNation))).
power(congress,defineAndPunish(piraciesAndFelonies(highSeas))).
power(congress,declare(war)).
power(congress,grant(letter(marqueAndReprisal))).
power(congress,make(rules(capturesOnLandAndWaters))).
power(congress,provideAndMaintain(navy)).
power(congress,calling(militia)).
power(congress,suppress(insurrection)).
power(congress,suppress(invasions)).
power(congress,organizeArmingDisciplining(militia)).
consent(congress,no_one,legislations).
area(newYork,10).
consent(congress,newYork,legislations).
consent(congress,newYork,authority).
power(congress,legislations(X)) :- area(X,Y) , Y=<10 , consent(congress,X,legislations). /* test case to be provided area of the region(X)  should be less than or equal to 10 and congress should have consent of the region X to do legislations*/
power(congress,authority([forts,magazines,arsenals],X)) :- consent(congress,X,authority). /*test case - congress should get the consent of region X to have authority*/
power(congress,laws(carrying(execution(power)))).

%Section 9

restriction(congress,not(prohibit(migrationImportation))).
accepted(ram,newYork).
yearOfImport(ram,1705).
imported(X,Y) :- accepted(X,Y),yearOfImport(X,Z),Z<1808.  /*test case- accepted(ram,newYork) yearOfImport(ram,1705)  gives tax(ram,5) as True*/
tax(X,Z) :- imported(X,Y), Z=<10.
event(rebellion).
power(congress,suspend(privilege(writOfHabeusCorpus))) :- event(rebellion);event(invasion);event(publicSafety). /*test case event(no)*/
restriction(congress,bill(attainder)).
restriction(congress,bill(exPostFactoLaw)).
restriction(congress,bill(capitalization)).
power(congress,taxDuty(articles)).  /* change made by amendment 16 */
restriction(congress,preferences('Regulation of Commerce or Revenue to the Ports of one State over those of another')).
restriction(congress,money(treasury)).
duty(congress,statement(expenditures)).
restriction(congress,appoint(others(positions))).

%Section 10

restriction(X,no(alliance)) :- stateOfUS(X).
restriction(X,grant(letter)) :- stateOfUS(X).
restriction(X,coin(money)) :- stateOfUS(X).
restriction(X,bill(attainder)) :- stateOfUS(X).
restriction(X,bill(exPostFactoLaw)) :- stateOfUS(X).
consent(congress,newYork,lay(importDuties)).
power(X,lay(importsDuties)) :- stateOfUS(X),consent(congress,X,lay(importDuties)). /*test case */


%Article 2

%Section 1

executivePower(presidentOfUS).
term(president,4).
term(vicePresident,4).
numberOfElectors(State, X):-  numberOfRepresentatives(State,Y), X is Y+2.
isElector(X):- not(X== senator), not(X== representative), not(X== personHoldingOfficeOfTrustOrProfit).

electionProcedureRole(electors, "The Electors shall meet in their respective States, and vote by Ballot for two Persons, of whom one at least shall not be an Inhabitant of the same State with themselves.").

electionProcedureRole(electors, "The Electors shall name in their ballots the person voted for as President, and in distinct ballots the person voted for as Vice-President, and they shall make distinct lists of all persons voted for as President, and of all persons voted for as Vice-President,     and of the number of votes for each ").  /*change due to amendment 12*/

electionProcedureRole(electors, "The Electors shall sign and certify the list, and transmit it to the Seat of the Government of the United States, directed to the President of the Senate.").
electionProcedureRole(president, "The President of the Senate shall, in the Presence of the Senate and House of Representatives, open all the Certificates, and the Votes shall then be counted.").

winningRuleWithMajority("The Person having the greatest Number of Votes shall be the President, if such Number be a Majority of the whole Number of Electors appointed; and if there be more than one who have such Majority, and have an equal Number of Votes, then the House of Representatives shall immediately choose by Ballot one of them for President").

winningRuleWithoutMajority("If no Person has a Majority, then from the persons having the highest numbers not exceeding three on the list of those voted for as President, the House of Representatives shall choose immediately, by ballot, the President."). /*change due to amendment 12*/

voters("The Votes shall be taken by States, the Representation from each State having one Vote; A quorum for this Purpose shall consist of a Member or Members from two thirds of the States, and a Majority of all the States shall be necessary to a Choice.").

vicePresidentChoice("After the Choice of the President, the Person having the greatest Number of Votes of the Electors shall be the Vice President. But if there should remain two or more who have equal Votes, the Senate shall choose from them by Ballot the Vice President.").

electionDayAndTime(determinedByCongress).
qualified(X, eligibleToOfficeOfPresident):- citizen(X,Y), Y>=14, age(X,Z), Z>=35.

qualified(X, eligibleToOfficeOfVicePresident):- citizen(X,Y), Y>=14, age(X,Z), Z>=35.

receive(president, "Compensation shall neither be increased nor diminished during the Period for which he shall have been elected").
restriction(president, "he shall not receive within that Period any other Emolument from the United States, or any of them.").
oathTakenByPresident("I do solemnly swear (or affirm) that I will faithfully execute the Office of President of the United States, and will to the best of my Ability, preserve, protect and defend the Constitution of the United States.").

%Section 2

commanderInChiefOfArmy(president).
commanderInChiefOfNavy(president).
commanderInChiefOfMilitiaOfSeveralStates(president).
power(president, grant(pardon(offenseAgainstUS))).
power(president, makeTreatiesWithConsentOfSenate).
consentAndAdvice(senate).
appoint(president, X):- consentAndAdvice(senate),  X== ambassadors; X== publicMinisters; X== consuls; X== judgeOfTheSupremeCourt.
power(president, fillUpAllVacancies(happen(duringTheRecessOfTheSenate))).


%Section 3

duty(president, "He shall from time to time give to the Congress Information of the State of the Union").
receive(president,ambassadorAndPublicMinister).
duty(president, "he shall take Care that the Laws be faithfully executed, and shall Commission all the Officers of the United States").

%Section 4

removeFromOffice(X, Crime):- X == president; X== vicePresident; X == civilOfficer, Crime == convictionOfTreaser;  Crime == convictionOfBribery;   Crime == convictionOfOtherHighCrimesAndMisdemeanors.


%Article 3

%Section 1

judicialPower(oneSupremeCourt).
duty(supremeCourtJudge,hold(office(goodBehaviour))).
duty(inferiorCourtJudge,hold(office(goodBehaviour))).
right(supremeCourtJudge,receive(compensation)).
right(inferiorCourtJudge,receive(compensation)).

%Section2

power(judicialPower,casesIn(Law)).
power(judicialPower,casesIn(Equity)).
power(judicialPower,casesIn(treaties)).
power(judicialPower,casesIn(ambassadors)).
power(judicialPower,casesIn(OtherPublicMinistersAndConsuls)).
power(judicialPower,casesIn(admiralty)).
power(judicialPower,casesIn(maritimeJurisdiction)).
restriction(judicialPower,controversiesbetweenCitizenOftwoOrMoreStates). /* Changed due to Amendment 11 */
need(supremeCourt, originalJurisdictionInAmbassadorsCase).
need(supremeCourt, originalJurisdictionInPublicMinistersCase).
need(supremeCourt, appellateJurisdictionInOtherCase).

trials(crimesExceptImpeachement, "Trial shall be held in the State where the said Crimes shall have been committed; but when not committed within any State, the Trial shall be at such Place or Places as the Congress may by Law have directed.").

%Section3
treason(levyingWarAgainstUS).
treason(adheringToTheirEnemies).
convictedFOfTreason(testimonyOfTwoWitnessesToOvertAct).
convictedOfTreason(confessionInOpenCourt).
power(congress, "Declare the Punishment of Treason, but no Attainder of Treason shall work Corruption of Blood, or Forfeiture except during the Life of the
Person attainted." ).


%Article 4

%Section 1

faithAndCredit(X,Y) :- stateOfUS(X), stateOfUS(Y).
reopenCase(X,Y):- not(faithAndCredit(X,Y)). /*discuss*/
power(congress, "By general Laws prescribe the Manner in which such Acts, Records and Proceedings shall be proved, and the Effect thereof ").


%Section2

hasPrivileges(X):- citizen(X,_).
hasImmunity(X):- citizen(X,_).
returnFleedPerson(X,Y):- from(stateOfUS(X)), to(stateOfUS(Y)).
law("Neither slavery nor involuntary servitude, except as a punishment for crime whereof the party shall have been duly convicted, shall exist within the United States, or any place subject to their jurisdiction. "). /* changes made by amendment XIII */

%Section3

newState(X):- consentCongress(X).
newState(X,Y):- consentCongress(Y), consentLegislatureOfStates(Y), X is part(stateOfUS(Y)).

% X is a newly formed state and Y is the state whose part is used to form X.
power(congress,"To dispose of and make all needful Rules and Regulations respecting the Territory or other Property belonging to the United States").

%Section4

protectionAgainstInvasion(X):- stateOfUS(X).
republicanFormOfGovt(X):- stateOfUS(X).

%Article5

proposeAmendments(congress, "Whenever two thirds of both Houses shall deem it necessary and shall be valid to all Intents and Purposes.").
coventionForProposingAmendments(congress, "Application of the Legislatures of two thirds of the several States and shall be valid to all Intents and Purposes.").
proposeModeOfRetification(congress, "When ratified by the Legislatures of three-fourths of the several States, or by Conventions in three fourths.").



%Article6

elementaryLaws("All Debts contracted and Engagements entered into, before the Adoption of this Constitution, shall be as valid against the United States under this Constitution, as under the Confederation").

elementaryLaws("This Constitution, and the Laws of the United States which shall be made in Pursuance thereof; and all Treaties made, or which shall be made, under the Authority of the United States, shall be the supreme Law of the Land.”).

elementaryLaws( “The Judges in every State shall be bound, any Thing in the Constitution or Laws of any State to the Contrary notwithstanding.").

boundByOathOrAffirmation("senators, representatives, members of state legislatures, all executive and judicial officers","to support this constitution but no religious test shall ever be required as a Qualification to any Office or public Trust under the United States.").

%Article7

sufficient("ratification of conventions of nine states establish the constitution between the states").

dateOfConvention(17,9,1787).

peoplePresentAtConvention("President","Go. Washington").
peoplePresentAtConvention("Deputy","Virginia").
peoplePresentAtConvention("NEW HAMPSHIRE","John langdon").
peoplePresentAtConvention("NEW HAMPSHIRE","Nicholas Gilman").
peoplePresentAtConvention("MASSACHUSETTS","Nathaniel Gorham").
peoplePresentAtConvention("MASSACHUSETTS","Rufus King").
peoplePresentAtConvention("CONNECTICUT","Wm. Saml. Johnson").
peoplePresentAtConvention("CONNECTICUT","Roger Sherman").
peoplePresentAtConvention("NEW YORK","Alexander Hamilton").
peoplePresentAtConvention("NEW JERSEY","Wil: Livingston").
peoplePresentAtConvention("NEW JERSEY","David Brearley").
peoplePresentAtConvention("NEW JERSEY","Wm. Paterson").
peoplePresentAtConvention("NEW JERSEY","Jona: Dayton").
peoplePresentAtConvention("PENNSYLVANIA","B Franklin").
peoplePresentAtConvention("PENNSYLVANIA","Thomas Mifflin").
peoplePresentAtConvention("PENNSYLVANIA","Robt Morris").
peoplePresentAtConvention("PENNSYLVANIA","Geo.Clymer").
peoplePresentAtConvention("PENNSYLVANIA","Thos. FitzSimons").
peoplePresentAtConvention("PENNSYLVANIA","Jared Ingersoll").
peoplePresentAtConvention("PENNSYLVANIA","James Wilson").
peoplePresentAtConvention("PENNSYLVANIA","Gouv Morris").
peoplePresentAtConvention("DELAWARE","Geo: Read").
peoplePresentAtConvention("DELAWARE","Gunning Bedford jun").
peoplePresentAtConvention("DELAWARE","John Dickinson").
peoplePresentAtConvention("DELAWARE","Richard Bassett").
peoplePresentAtConvention("DELAWARE","Jaco: Broom").
peoplePresentAtConvention("MARYLAND","James McHenry").
peoplePresentAtConvention("MARYLAND","Dan of St. Thos. Jenifer").
peoplePresentAtConvention("MARYLAND","Danl Carroll").
peoplePresentAtConvention("VIRGINIA","John Blair-").
peoplePresentAtConvention("VIRGINIA","James Madison Jr.").
peoplePresentAtConvention("NORTH CAROLINA","Wm. Blount").
peoplePresentAtConvention("NORTH CAROLINA","Richd. Dobbs Spaight").
peoplePresentAtConvention("NORTH CAROLINA","Hu Williamson").
peoplePresentAtConvention("SOUTH CAROLINA","J. Rutledge").
peoplePresentAtConvention("SOUTH CAROLINA","Charles Cotesworth Pinckney").
peoplePresentAtConvention("SOUTH CAROLINA","Charles Pinckney").
peoplePresentAtConvention("SOUTH CAROLINA","Pierce Butler").
peoplePresentAtConvention("GEORGIA","William Few").
peoplePresentAtConvention("GEORGIA","Abr Baldwin").
peoplePresentAtConvention("Secretary","William Jackson").


%Preamble of Rights
billOfRights(15,10,1791).
restrictiveClauses(preventAbuseofPower).
valid(ratified(threeFourth(legislatures))).
valid(pass(twoThird(bothHouses))).


%Amendment 1

amendment1(ratified(15,12,1791)).
restriction(congress,establish(religion)).
restriction(congress,prohibit(freeExercise)).
restriction(congress,abridge(freedomOfSpeech)).
restriction(congress,abridge(freedomOfPress)).
restriction(congress,abridge(rightToPeacefullyAssemble)).
restriction(congress,petition(government(rednessOfGrievances))).

%Amendment 2

amendment2(ratified(15,12,1791)).
right(X,keep_and_bear_arms):-citizen(X,Y),Y>=18.

%Amendment 3

amendment3(ratified(15,12,1791)).
privilege(soldier,duringWar(takeQuaterWithLaw)).
restriction(soldier,duringPeace(canNotTakeQuater)).

%Amendment 4

amendment4(ratified(15,12,1791)).
right(X,secure(against(unreasonable(searchesAndSeisures)))) :- citizen(X,Y).

%Amendment 5

amendment5(ratified(15,12,1791)).
right(X,secure(against(twicePunishment(sameCrime)))) :- citizen(X,Y).
right(X,"shall not be compelled in any criminal case to be a witness against himself, nor be deprived of life, liberty, or property, without due process of law; nor shall private property be taken for public use, without just compensation.") :- citizen(X,Y).

%Amendment 6

amendment6(ratified(15,12,1791)).
right(accused,speedy_and_public_trial).

%Amendment 7

amendment7(ratified(15,12,1791)).
right(jury,cannotReExaminFactsInAnyCourtIfAmountExceeds20dollars).

%Amendment 8

amendment8(ratified(15,12,1791)).
right(X,noExcessiveFines) :- citizen(X,Y).
right(X,noCruelPunish) :- citizen(X,Y).

%Amendment 9

amendment9(ratified(15,12,1791)).
right(X,cannot(deny(constitutionalRights))) :- citizen(X,Y).

%Amendment 10

amendment10(ratified(15,12,1791)).
power(X,powerNotMentionedInConstitution) :- stateOfUS(X).

%Amendment 11

amendmentXI(pass(4,3,1794)).
amendmentXI(ratified(7,2,1795)).
/* Change made in Article 3 section 2 */

%Amendment 12

amendmentXII(pass(9,12,1803)).
amendmentXII(ratified(15,5,1804)).
presidentNotChosen(president(vicePresident(fourthDayOfFollowingMarch))).
/* change due to amendment 20 */

%Amendment 13

amendmentXIII(pass(31,1,1865)).
amendmentXIII(ratified(6,12,1865)).

%Section 1

/* Changes made in article IV section 1 */

%Section 2

power(congress,enforce(articleIV)).

%Amendment 14

% Section1

bornUS(X) :- citizen(X,_).
stateCannot(abridgePrividegesOfUSCitizens).
stateCannot(depriveLibertyandPropertyWithoutLaw).
stateCannot(denyJuridictionOfEqualProtectionOfLaws).

% Section2

equalRightToVoteForMale(X) :- notCriminal(X),notRebellian(X),gender(X,male),citizen(X,Y),age(X,Z),Y>=21,Z>=18.
/* change due to amendment 26 Section 1 */

% Section3

previouslyElected(X,cantBeChosenAgainToSupporConstitution) :-
havingLessThanTwoThirdVotesInHouse(X),rebel(X).

% Section4

restriction(law,"United States nor any State shall assume or pay any debt or obligation incurred in aid of insurrection or rebellion against the United States, or any claim for the loss or emancipation of any slave; but all such debts, obligations and claims shall be held illegal and void. ").
privilege(law,"validity of the public debt of the United States, authorized by law, including debts incurred for payment of pensions and bounties for services in suppressing insurrection or rebellion, shall not be questioned").

% Section5
power(congress,enforce(amendment14)).

%Amendment 15

amendmentXV(pass(26,2,1869)).
amendmentXV(ratified(3,2,1870)).

%Section 1

right(X,vote(regardless(raceColourServitude))) :- citizen(X,Y).

%Section 2

power(congress,enforce(amendment15)).

%Amendment 16

amendmentXVI(pass(2,7,1909)).
amendmentXVI(ratified(3,2,1913)).
/* change made in article 1 section 9 */

%Amendment 17

amendmentXVII(pass(13,5,1912)).
amendmentXVII(ratified(8,4,1913)).
/* change made in article 1  section 3 */

%Amendment18

amendmentXVIII(pass(18,12,1917)).
amendmentXVIII(ratified(16,1,1919)).
amendmentXVIII(repealed(amendment21)).
% Section1

prohibit(manufacturingTransportingAndSaleOfIntoxicatingLiquorsWithinUS).

% Section2
power(congress,enforce(amendment18)).

% Section3
durationOfArticle("7years").

% Amendment19

amendmentXVIII(pass(4,6,1919)).
amendmentXVIII(ratified(18,8,1920)).
notToDenyCitizenVoteRights(sex).
power(congress,enforce(amendment19)).

%Amendment 20
amendmentXX(pass(2,3,1932)).
amendmentXX(ratified(23,1,1933)).

%Section 1
endOfTerm(president, "20thJanuary").
endOfTerm(vicePresident, "20thJanuary").
senator(ram). /*test case*/
representative(shyam). /*test case*/
endOfTerm(X,"3rdJanuary"). :- senator(X).
endOfTerm(X,"3rdJanuary"). :- representative(X).
conditionendOfTerm(articleNotRatified).
beginTerm(succesor(X)) :- endOfTerm(X).


%Section 2
meetingCongress(onceEveryYear).
dayOfEffectOfMeeting("3rdDayofJanuary").
/* Changes made in article 1 section 4 */

%Section 3

responsibility(president,vicePresident) :- dead(president);notChosen(president);notQualify(president).
responsibility(president,congress) :- notQualify(president),notQualify(vicePresident).

%Section 4

power(congress,choosePresident).
power(senate,chooseVicePresident).



%Section 5
effectDate(section1,"15thOctober").
effectDate(section2,"15thOctober").

%Section 6
operative(20,"Amendment shall be ratified by three-fourth legislature in less than 7 years").


% Amendment 22
amendmentXXII(pass(21,3,1947)).
amendmentXXII(ratified(27,2,1951)).

% section 1
term(president,X):- X is 1;X is 2.
term(W,3).
notElected(president,W).
office(president,W).
numberOfTerms(X,Z) :- notElected(president,X),office(president,X),term(X,Y), Y>2.

% Current year is assumed to be 2019,so the remainder of the section does not apply.

%Section 2
%Ratification of the article makes this Section invalid

%Amendment 23

amendmentXXIII(pass(16,6,1960)).
amendmentXXIII(ratified(29-3-1961)).

%Section 1

district(columbia).
population(State,Population) :- stateOfUS(State).
electorate(District,Number):-district(District),senator(District,Num1),representative(District,Num2), Number is Num1+Num2.
leastPopulousState(X) :-stateOfUS(X).
electorFromDistrict(xyz).
function(X,['election of President and Vice President','shall meet in the District and perform such duties as provided by the Article 12 of amendment']) :-electorFromDistrict(X).
%a self made function of district electors has been added.

%Section 2
enforce(amendment23).
power(congress,enforce(amendment23)).

%Amendment 24

amendmentXXIV(pass(27,8,1962)).
amendmentXXIV(ratified(23,1,1964)).

%section 1
tax(Citizen,paid):-citizen(Citizen,_).
tax(Citizen,default):-citizen(Citizen,_).
right(Citizen,vote) :-tax(Citizen,paid);tax(Citizen,default).

%section 2

enforce(amendment24).
power(congress,enforce(amendment24)).

%Amendment 25

amendmentXXV(pass(6,7,1965)).
amendmentXXV(ratified(10,2,1967)).
writtenForDeclearation(president, "unable to discharge powers and duties related to his office").
writtenForDeclearation(vicePresident, "President is unable to discharge powers and duties related to his office").
writtenForDeclearation(executive, "President is unable to discharge powers and duties related to his office").

%section 1

newElectedPresident(vicePresident) :- resignation(president);dies(president);removedFromOffice(president).

%section 2

nominate(vicePresident) :- consentFromCongress(true),vacancy(vicePresident).

%section 3

actingPresident(vicePresident) :- writtenForDecleration(vicePresident,_).

%section 4

actingPresident(vicePresident) :- writtenForDecleration(executive,_),writtenForDecleration(vicePresident,_).

compensate(president).
oathByPresident("I do solemnly swear (or affirm) that I will faithfully execute the Office of President of the United States, and will to the best of my Ability, preserve, protect and defend the Constitution of the United States.").

%Amendment 26

amendmentXXVI(pass(23,3,1971)).
amendmentXXVI(ratified(1,7,1971)).

%Section 1
vote(X) :- age(X,Y),Y>18.

/* Changes made in amendment 14 section 2 */

%Section 2

power(congress,enforce(appropriateLegislation)).

%Amendment 27

amendmentXXVII(pass(25,9,1789)).
amendmentXXVII(ratified(7,5,1992)).
prohibition(congress,change_law(compensationOfService(senator))).
prohibition(congress,change_law(compensationOfService(representative))).

%END



