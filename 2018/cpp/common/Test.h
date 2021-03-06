#pragma once

#include <iostream>
#include <sstream>
#include <vector>
#include <unordered_set>
#include <cassert>

class Test
{
public:

    class DeferredAssertionScope
    {
    public:
        DeferredAssertionScope(void)  { DeferAssertions(); }
        ~DeferredAssertionScope(void) { ContinueAssertions(); }
    };

#   define TEST_SCOPE_DEFER_ASSERTIONS Test::DeferredAssertionScope TEST_SCOPE_DEFER = Test::DeferredAssertionScope();

    template <typename T>
    inline static void AssertVerbose(const T & actual, const T & expected, std::string msg = "", std::string name = "")
    {
        std::cout << "\n" << (name.empty() ? "Test" : name) << ": Expected '" << expected << "', Actual '" << actual << "'\n";
        if (actual != expected)
        {
            if (!msg.empty()) std::cout << "ERROR: " << msg << "\n";
            std::cout << "ERROR: Assertion failed, actual result: " << actual << "\n\n";
            PerformAssertion(actual == expected);
        }
    }

    template <typename TContainer>
    inline static void AssertIterableVerbose(const TContainer & actual, const TContainer & expected, std::string msg = "", std::string name = "")
    {
        std::cout << "\n" << (name.empty() ? "Test" : name) << ": Expected '" << IterableToString(expected.cbegin(), expected.cend())
            << "', Actual '" << IterableToString(actual.cbegin(), actual.cend()) << "'\n";

        if (actual != expected)
        {
            if (!msg.empty()) std::cout << "ERROR: " << msg << "\n";
            std::cout << "ERROR: Assertion failed, actual result: " << IterableToString(actual.cbegin(), actual.cend()) << "\n\n";
            PerformAssertion(false);
        }
    }

    template <typename T>
    inline static void AssertVectorVerbose(const std::vector<T> & actual, const std::vector<T> & expected, std::string msg = "", std::string name = "")
    {
        std::cout << "\n" << (name.empty() ? "Test" : name) << ": Expected '" << IterableToString(expected.cbegin(), expected.cend()) 
            << "', Actual '" << IterableToString(actual.cbegin(), actual.cend()) << "'\n";

        if (!AssertVectorEqual(actual, expected))
        {
            if (!msg.empty()) std::cout << "ERROR: " << msg << "\n";
            std::cout << "ERROR: Assertion failed, actual result: " << IterableToString(actual.cbegin(), actual.cend()) << "\n\n";
            PerformAssertion(false);
        }
    }

    template <typename T>
    inline static void AssertComplexVerbose(const T & actual, const T & expected, std::string msg = "", std::string name = "")
    {
        std::cout << "\n" << (name.empty() ? "" : (name + ", ")) << "Expected:\n\n" << expected << "\n\n" << 
                             (name.empty() ? "" : (name + ", ")) << "Actual:\n\n" << actual << "\n";

        if (actual != expected)
        {
            if (!msg.empty()) std::cout << "ERROR: " << msg << "\n";
            std::cout << "ERROR: Assertion failed\n\n";
            PerformAssertion(actual == expected);
        }
    }
    
private:

    template <typename TIter>
    inline static std::string IterableToString(TIter start, TIter end)
    {
        std::stringstream ss;

        ss << "{ ";
        std::for_each(start, end, [&ss](const auto & el) { ss << el << " "; });
        ss << '}';

        return ss.str();
    }

    template <typename T>
    inline static bool AssertVectorEqual(const std::vector<T> & v0, const std::vector<T> & v1)
    {
        size_t n = v0.size();
        if (n != v1.size()) 
        {
            std::cout << "Vectors are not equal; |v0| == " << v0.size() << ", |v1| == " << v1.size() << "\n";
            return false;
        }

        for (size_t i = 0U; i < n; ++i)
        {
            if (v0[i] != v1[i])
            {
                std::cout << "Vectors are not equal; v0[" << i << "] == " << v0[i] << ", v1[" << i << "] == " << v1[i] << "\n";
                return false;
            }
        }

        return true;
    }
    

    inline static void DeferAssertions(void) { AssertionIsDeferred = true; }
    inline static void ContinueAssertions(void) 
    { 
        auto assertions = DeferredAssertions;
        DeferredAssertions.clear();

        AssertionIsDeferred = false; 

        for (auto assertion : assertions)
        {
            assert(assertion && "Deferred assertions");
        }
    }
    

private:

    inline static bool AssertionIsDeferred = false;
    inline static std::vector<bool> DeferredAssertions;


    inline static void PerformAssertion(bool assertion)
    {
        if (AssertionIsDeferred)
        {
            DeferredAssertions.push_back(assertion);
            return;
        }

        assert(assertion);
    }

};