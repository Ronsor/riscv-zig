volatile long long test() {
    volatile long long y = 6;
    volatile long long p = 7;
    return (y - p) == 13 ? 86 : 54;
}
