#define _GNU_SOURCE
#include <sched.h>

int cpu_setsize = CPU_SETSIZE;
int cpu_mask_size = sizeof(cpu_set_t);

int get_cpu_affinity_mask(cpu_set_t *mask)
{
    return sched_getaffinity(0, CPU_SETSIZE, mask);
}

int set_cpu_affinity_mask(cpu_set_t *mask)
{
    return sched_setaffinity(0, CPU_SETSIZE, mask);
}

void zero_cpu_affinity_mask(cpu_set_t *mask)
{
    CPU_ZERO(mask);
}

void set_cpu_affinity(int cpu, cpu_set_t *mask)
{
    CPU_SET(cpu, mask);
}

int cpu_affinity_p(int cpu, cpu_set_t *mask)
{
    return CPU_ISSET(cpu, mask);
}

void clear_cpu_affinity(int cpu, cpu_set_t *mask)
{
    CPU_CLR(cpu, mask);
}

    
    
