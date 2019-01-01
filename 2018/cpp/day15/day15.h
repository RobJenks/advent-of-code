#pragma once

#include "../base/AOCSolution.h"
#include "Actor.h"
class Combat;

class Day15 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void RunTests(void) const;
    void TestMapLoading(void) const;
    void TestReadingOrder(void) const;
    void TestSampleMovement(void) const;
    void TestSampleCombat(void) const;
    void TestFullScenarios(void) const;
    void TestAttackPower(void) const;

    void Part1(void) const;
    void Part2(void) const;

    bool EvaluateToConclusionWithoutCasualties(Combat & combat, Actor::Class faction, int combat_strength) const;
};
