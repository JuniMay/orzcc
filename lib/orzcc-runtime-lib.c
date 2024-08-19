#define _GNU_SOURCE
#include <stdio.h>
#include <sched.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <pthread.h>

#define THREAD_STACK_SIZE (1024 * 1024)
#define THREAD_COUNT 4
// #define DEBUG

typedef union
{
    int i;
    float f;
    void *p;
} var_data;

typedef struct
{
    var_data *elements;
    size_t size;
    size_t capacity;
    int start;
    int end;
    int ret;
} var_list;

var_list *orzcc_init_var_list(int capacity)
{
    var_list *list = malloc(sizeof(var_list));
    list->elements = malloc(capacity * sizeof(var_data));
    list->size = 0;
    list->capacity = capacity;

    return list;
}

void orzcc_free_var_list(var_list *list)
{
    free(list->elements);
    free(list);
}

void orzcc_var_list_push_int(var_list *list, int data)
{
    list->elements[list->size].i = data;
    list->size++;
}

void orzcc_var_list_push_float(var_list *list, float data)
{
    list->elements[list->size].f = data;
    list->size++;
}

void orzcc_var_list_push_ptr(var_list *list, void *data)
{
    list->elements[list->size].p = data;
    list->size++;
}

int orzcc_var_list_get_int(var_list *list, int index)
{
    return list->elements[index].i;
}

float orzcc_var_list_get_float(var_list *list, int index)
{
    return list->elements[index].f;
}

void *orzcc_var_list_get_ptr(var_list *list, int index)
{
    return list->elements[index].p;
}

int orzcc_var_list_get_start(var_list *list)
{
    return list->start;
}

int orzcc_var_list_get_end(var_list *list)
{
    return list->end;
}

void orzcc_var_list_ret_int(var_list *list, int data)
{
    list->ret = data;
}

var_list *orzcc_va_list_copy(var_list *list)
{
    var_list *new_list = orzcc_init_var_list(list->capacity);
    for (int i = 0; i < list->size; i++)
    {
        new_list->elements[i] = list->elements[i];
    }
    new_list->size = list->size;
    return new_list;
}

#ifdef NO_PTHREAD
pid_t orzcc_thread_create(int (*thread_start)(void *), var_list *arg)
{
    char *stack = malloc(THREAD_STACK_SIZE);
#ifdef DEBUG
    if (stack == NULL)
    {
        perror("malloc stack");
        raise(SIGSEGV);
    }
#endif
    pid_t thread_id = clone(thread_start, stack + THREAD_STACK_SIZE, (unsigned long)(SIGCHLD | CLONE_VM | CLONE_FS | CLONE_FILES | CLONE_SIGHAND | CLONE_THREAD | CLONE_SYSVSEM), arg);
#ifdef DEBUG
    if (thread_id == -1)
    {
        free(stack);
        perror("clone");
        raise(SIGSEGV);
    }
#endif
    return thread_id;
}

int orzcc_thread_join(pid_t thread_id)
{
    int status;
    int re = waitid(P_PID, 0, NULL, 0);
    printf("%d", re);
    return status;
}

// parallel loop within [from, to) with THREAD_COUNT threads
// it calls thread_start with arg, where arg.start and arg.end are the range for the thread
void orzcc_parallel_for(int from, int to, int (*thread_start)(void *), var_list *arg)
{
    if (to <= from)
    {
        return;
    }
    pid_t thread_ids[THREAD_COUNT];
    var_list *thread_args[THREAD_COUNT];
    // divide the range into THREAD_COUNT parts
    int step = (to - from + THREAD_COUNT - 1) / THREAD_COUNT;
    for (int i = 0; i < THREAD_COUNT; i++)
    {
        int start = from + i * step;
        int end = from + (i + 1) * step;
        if (end > to)
        {
            end = to;
        }
        thread_args[i] = orzcc_va_list_copy(arg);
        thread_args[i]->start = start;
        thread_args[i]->end = end;
        printf("Thread %d: start %d, end %d\n", i, thread_args[i]->start, thread_args[i]->end);
        thread_ids[i] = orzcc_thread_create(thread_start, thread_args[i]);
    }
    for (int i = 0; i < THREAD_COUNT; i++)
    {
        int status = orzcc_thread_join(thread_ids[i]);
        printf("Thread %d exited with %d\n", thread_ids[i], status);
        orzcc_free_var_list(thread_args[i]);
    }
}

#else

typedef struct
{
    pthread_t threads[THREAD_COUNT];
    pthread_mutex_t lock;
    pthread_cond_t cond;
    pthread_barrier_t barrier;
    void *(*tasks[THREAD_COUNT])(void *);
    var_list *args[THREAD_COUNT];
    int task_available[THREAD_COUNT];
    int terminate;
} threadpool_t;

static threadpool_t *pool;

static void *threadpool_worker(void *arg)
{
    int id = (intptr_t)arg;
    while (1)
    {
        pthread_mutex_lock(&pool->lock);
        while (!pool->task_available[id] && !pool->terminate)
        {
            pthread_cond_wait(&pool->cond, &pool->lock);
        }

        if (pool->terminate)
        {
            pthread_mutex_unlock(&pool->lock);
            break;
        }

        void *(*task)(void *) = pool->tasks[id];
        var_list *args = pool->args[id];
        pool->task_available[id] = 0;
        pthread_mutex_unlock(&pool->lock);

        if (task != NULL)
        {
            task(args);
        }

        pthread_barrier_wait(&pool->barrier);
    }
    return NULL;
}

void orzcc_init() __attribute__((constructor));

void orzcc_init()
{
    pool = malloc(sizeof(threadpool_t));
    pthread_mutex_init(&pool->lock, NULL);
    pthread_cond_init(&pool->cond, NULL);
    pthread_barrier_init(&pool->barrier, NULL, THREAD_COUNT + 1);
    pool->terminate = 0;

    for (int i = 0; i < THREAD_COUNT; i++)
    {
        pool->task_available[i] = 0;
        pthread_create(&pool->threads[i], NULL, threadpool_worker, (void *)(intptr_t)i);
    }
}

void orzcc_destroy() __attribute__((destructor));

void orzcc_destroy()
{
    pthread_mutex_lock(&pool->lock);
    pool->terminate = 1;
    pthread_cond_broadcast(&pool->cond);
    pthread_mutex_unlock(&pool->lock);

    for (int i = 0; i < THREAD_COUNT; i++)
    {
        pthread_join(pool->threads[i], NULL);
    }

    pthread_mutex_destroy(&pool->lock);
    pthread_cond_destroy(&pool->cond);
    pthread_barrier_destroy(&pool->barrier);
    free(pool);
}

void orzcc_parallel_for(int from, int to, void *(*thread_start)(void *), var_list *arg)
{
    int step = (to - from + THREAD_COUNT - 1) / THREAD_COUNT;

    pthread_mutex_lock(&pool->lock);
    for (int i = 0; i < THREAD_COUNT; i++)
    {
        int start = from + i * step;
        int end = from + (i + 1) * step;
        if (end > to)
        {
            end = to;
        }

        pool->tasks[i] = thread_start;
        pool->args[i] = orzcc_va_list_copy(arg);
        pool->args[i]->start = start;
        pool->args[i]->end = end;
        pool->task_available[i] = 1;
    }
    pthread_cond_broadcast(&pool->cond);
    pthread_mutex_unlock(&pool->lock);

    pthread_barrier_wait(&pool->barrier);

    for (int i = 0; i < THREAD_COUNT; i++)
    {
        orzcc_free_var_list(pool->args[i]);
    }
    orzcc_free_var_list(arg);
}

int orzcc_parallel_for_reduce_add_int(int from, int to, void *(*thread_start)(void *), var_list *arg, int init)
{
    int step = (to - from + THREAD_COUNT - 1) / THREAD_COUNT;

    pthread_mutex_lock(&pool->lock);
    for (int i = 0; i < THREAD_COUNT; i++)
    {
        int start = from + i * step;
        int end = from + (i + 1) * step;
        if (end > to)
        {
            end = to;
        }

        pool->tasks[i] = thread_start;
        pool->args[i] = orzcc_va_list_copy(arg);
        pool->args[i]->start = start;
        pool->args[i]->end = end;
        pool->task_available[i] = 1;
    }
    pthread_cond_broadcast(&pool->cond);
    pthread_mutex_unlock(&pool->lock);

    pthread_barrier_wait(&pool->barrier);

    for (int i = 0; i < THREAD_COUNT; i++)
    {
        init += pool->args[i]->ret;
        orzcc_free_var_list(pool->args[i]);
    }
    orzcc_free_var_list(arg);

    return init;
}
#endif

// void *test(void *arg)
// {
//     // test arg passing
//     int hello = orzcc_var_list_get_int(arg, 0);
//     if (hello != 114514)
//     {
//         printf("Thread %ld: hello is %d, expect 114514\n", pthread_self(), hello);
//     }

//     // addup from start to end
//     int start = orzcc_var_list_get_start(arg);
//     int end = orzcc_var_list_get_end(arg);
//     printf("Thread %ld: start %d, end %d\n", pthread_self(), start, end);
//     int sum = 0;
//     for (int i = start; i < end; i++)
//     {
//         sum += i;
//     }
//     printf("Thread %ld: sum %d\n", pthread_self(), sum);
//     orzcc_var_list_ret_int(arg, sum);
// }

// int main()
// {
//     var_list *arg = orzcc_init_var_list(1);
//     orzcc_var_list_push_int(arg, 114514);
//     int sum = orzcc_parallel_for_reduce_add_int(0, 10000, test, arg, 0);
//     // orzcc_parallel_for(0, 10000, test, arg);
//     // orzcc_free_var_list(arg);

//     printf("Sum: %d\n", sum);

//     return 0;
// }
