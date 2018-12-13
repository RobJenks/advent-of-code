#pragma once

#include "../base/AOCSolution.h"
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <sstream>
#include <array>

class Day4 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void Part1(void) const;
    void Part2(void) const;


    static const int UNKNOWN_GUARD = -1;
    static const int ANY_GUARD = -1;


    enum class EventType { Unknown = 0, BeginShift, FallAsleep, WakeUp };
    static std::string EventTypeString(EventType event_type);

    class EventData
    {
    public:
        int Guard;
        EventType Type;

        inline EventData(void)                      : EventData(EventType::Unknown, UNKNOWN_GUARD) { }
        inline EventData(EventType type)            : EventData(type, UNKNOWN_GUARD) { }
        inline EventData(EventType type, int guard) : Type(type), Guard(guard) { }
    };

    // Represents a milestone in the event sequence, valid until the next chronologically-ordered event
    class Event
    {
    public:
        int Month, Day, Hour, Minute;
        EventData Data;

        inline Event(void) : Month(0), Day(0), Hour(0), Minute(0), Data() { }

        inline bool operator<(const Event & other) const
        {
            if (Month != other.Month) return (Month < other.Month);
            if (Day != other.Day) return (Day < other.Day);
            if (Hour != other.Hour) return (Hour < other.Hour);
            return (Minute < other.Minute);
        }

        inline std::string str(void) const
        {
            std::ostringstream ss;
            ss << Month << "/" << Day << " " << Hour << ":" << Minute << " [Guard: "
                << Data.Guard << ", Event: " << EventTypeString(Data.Type);
            return ss.str();
        }
    };

    static const std::string EV_DESC_ASLEEP;
    static const std::string EV_DESC_AWAKE;

    std::vector<Event> ConstructTimeline(const std::vector<std::string> & event_strings) const;

    Event ParseEvent(const std::string & event_string) const;
    EventData ParseEventData(const std::string & event_data) const;

    // Returns the time (in minutes) between events.  Assumes ev0 occurs before or at the same time as ev1
    int TimeBetweenSleepEvents(const Event & ev0, const Event & ev1) const;

    std::unordered_map<int, int> GetGuardTimeAsleep(const std::vector<Event> & events) const;

    // Returns the sleep distribution across each minute of 00:00-00:59 for a particular guard
    std::unordered_map<int, std::array<int, 60U>> GetSleepDistribution(const std::vector<Event> & events, int guard = ANY_GUARD) const;

    // Returns the set of unique guard IDs in the event sequence

};