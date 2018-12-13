#include "day4.h"
#include <iostream>

const std::string Day4::EV_DESC_ASLEEP = "falls asleep";
const std::string Day4::EV_DESC_AWAKE = "wakes up";

void Day4::Run(void) const
{
    std::cout << "\nDay 4:\n";

    Part1();
    Part2();
}

void Day4::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day4/input.txt")));
    std::vector<Event> events = ConstructTimeline(input);
    
    std::unordered_map<int, int> sleeptime = GetGuardTimeAsleep(events);

    // Find guard with maximum sleep time
    int guard = UNKNOWN_GUARD; int maxtime = 0;
    for (const auto & sleep : sleeptime)
    {
        if (sleep.second > maxtime)
        {
            guard = sleep.first;
            maxtime = sleep.second;
        }
    }
    
    // Find hour covered by most sleep periods for this guard
    int minute = -1; int eventcount = 0;
    auto dist_map = GetSleepDistribution(events, guard);
    const auto & dist = dist_map[guard];

    for (int i = 0; i < 60; ++i)
    {
        if (dist[i] > eventcount)
        {
            minute = i;
            eventcount = dist[i];
        }
    }

    std::cout << "Part 1 result = " << (guard * minute) << " (Guard #" << guard << " * Minute " << minute << ")\n";
}

void Day4::Part2(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day4/input.txt")));
    std::vector<Event> events = ConstructTimeline(input);

    auto dist = GetSleepDistribution(events);

    int guard = UNKNOWN_GUARD; int minute = -1; int maxevents = 0;
    for (const auto & entry : dist)
    {
        for (int i = 0; i < 60; ++i)
        {
            if (entry.second[i] > maxevents)
            {
                guard = entry.first;
                minute = i;
                maxevents = entry.second[i];
            }
        }
    }

    std::cout << "Part 2 result = " << (guard * minute) << " (Guard #" << guard << " * Minute " << minute << ")\n";
}

std::vector<Day4::Event> Day4::ConstructTimeline(const std::vector<std::string> & event_strings) const
{
    // Build chronologically-sorted vector of events
    std::vector<Event> events;
    for (const auto & event_string : event_strings)
    {
        Event ev = ParseEvent(event_string);
        events.insert(std::lower_bound(events.begin(), events.end(), ev), ev);
    }

    // Propagate guard information from each 'begin shift' to the next shift milestone, since this
    // data is otherwise not available for intermediate events
    for (auto & ev : events)
    {
        if (ev.Data.Guard == UNKNOWN_GUARD) ev.Data.Guard = (&ev - 1)->Data.Guard;
    }

    return events;
}

Day4::Event Day4::ParseEvent(const std::string & event_string) const
{
    Event ev;
    char csink; int isink;
    std::string desc;

    std::stringstream ss(event_string);

    ss >> csink >> isink >> csink >> ev.Month >> csink >> ev.Day >> ev.Hour
        >> csink >> ev.Minute >> csink;
    
    std::getline(ss, desc);
    desc.erase(0, desc.find_first_not_of(" "));
    ev.Data = ParseEventData(desc);

    return ev;
}

Day4::EventData Day4::ParseEventData(const std::string & event_data) const
{
    if (event_data == EV_DESC_ASLEEP) return EventData(EventType::FallAsleep);
    if (event_data == EV_DESC_AWAKE) return EventData(EventType::WakeUp);

    int guard;
    char csink; std::string ssink;
    std::stringstream ss(event_data);

    ss >> ssink >> csink >> guard;

    return EventData(EventType::BeginShift, guard);
}

std::string Day4::EventTypeString(EventType event_type)
{
    switch (event_type)
    {
        case EventType::BeginShift:         return "BeginShift";
        case EventType::FallAsleep:         return "FallAsleep";
        case EventType::WakeUp:             return "WakeUp";
        default:                            return "Unknown";
    }
}

std::unordered_map<int, int> Day4::GetGuardTimeAsleep(const std::vector<Day4::Event> & events) const
{
    // Map (Guard -> TotalTimeAsleep).  Slightly lazy to use a map but convenient
    std::unordered_map<int, int> asleep;
    for (const Event & ev : events)
    {
        if (ev.Data.Type == EventType::WakeUp)
        {
            int totaltime = asleep[ev.Data.Guard];
            totaltime += TimeBetweenSleepEvents(*(&ev - 1), ev);
            asleep[ev.Data.Guard] = totaltime;
        }
    }

    return asleep;
}

// Returns the time (in minutes) between events.  Assumes ev0 occurs before or at the same time as ev1
int Day4::TimeBetweenSleepEvents(const Day4::Event & ev0, const Day4::Event & ev1) const
{
    return (ev1.Minute - ev0.Minute);       // Since sleep events are constrained to 00:00-00:59 only 
}

// Returns the sleep distribution across each minute of 00:00-00:59 for a particular guard
std::unordered_map<int, std::array<int, 60U>> Day4::GetSleepDistribution(const std::vector<Day4::Event> & events, int guard) const
{
    std::unordered_map<int, std::array<int, 60U>> dist;

    for (const Event & ev : events)
    {
        if ((guard = ev.Data.Guard || guard == ANY_GUARD) && ev.Data.Type == EventType::WakeUp)
        {
            if (dist.find(ev.Data.Guard) == dist.end())
                dist[ev.Data.Guard] = { 0 };

            for (int min = (&ev - 1)->Minute; min < ev.Minute; ++min) 
                ++dist[ev.Data.Guard][min];
        }
    }

    return dist;
}



