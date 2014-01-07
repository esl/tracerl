#include <stdio.h>
#include <time.h>
#include "erl_driver.h"
#include "ei.h"

/*********************************************************************
 * ErlIOVec manipulation functions - stolen from efile_drv.c
 */

/* char EV_CHAR_P(ErlIOVec *ev, int p, int q) */
#define EV_CHAR_P(ev, p, q)			\
  (((char *)(ev)->iov[q].iov_base) + (p))

/* int EV_GET_CHAR(ErlIOVec *ev, char *p, int *pp, int *qp) */
#define EV_GET_CHAR(ev, p, pp, qp) efile_ev_get_char(ev, p ,pp, qp)
static int
efile_ev_get_char(ErlIOVec *ev, char *p, size_t *pp, size_t *qp) {
  if (*pp + 1 <= ev->iov[*qp].iov_len) {
    *p = *EV_CHAR_P(ev, *pp, *qp);
    if (*pp + 1 < ev->iov[*qp].iov_len)
      *pp += 1;
    else {
      *qp += 1;
      *pp = 0;
    }
    return !0;
  }
  return 0;
}

/*********************************************************************/

int foo(int x) {
  return x+1;
}

int bar(int y) {
  int t = (int) time(NULL);
  while(time(NULL) - t < 2);
  return y*2;
}

int do_f(char fn, char arg) {
  if (fn == 1) {
    return foo(arg);
  } else if (fn == 2) {
    return bar(arg);
  } else {
    return -1;
  }
}

typedef struct {
  ErlDrvPort port;
} test_data;

typedef struct {
  char arg, fn, res;
} test_async_data;

static int test_drv_init() {
  return 0;
}

void test_drv_finish() { }

static ErlDrvData test_drv_start(ErlDrvPort port, char *buff) {
  test_data* d = (test_data*)driver_alloc(sizeof(test_data));
  d->port = port;
  return (ErlDrvData)d;
}

static void test_drv_stop(ErlDrvData handle) {
  driver_free((char*)handle);
}

static void do_async(void* async_data) {
  test_async_data * ad = (test_async_data *) async_data;
  ad->res = (char) do_f(ad->fn, ad->arg);
}

static void test_drv_output(ErlDrvData handle, char *buff,
                            ErlDrvSizeT bufflen) {
  test_data* d = (test_data*)handle;
  test_async_data* ad = (test_async_data*)driver_alloc(sizeof(test_async_data));
  char method = buff[0], fn = buff[1], arg = buff[2], res;
  switch (method) {
  case 1:
    res = (char) do_f(fn, arg);
    driver_output(d->port, &res, 1);
    break;
  case 2:
    ad->fn = fn;
    ad->arg = arg;
    driver_async(d->port, NULL, do_async, (void *) ad, NULL);
  }
}

static void test_drv_outputv(ErlDrvData handle, ErlIOVec *ev) {
  test_data* d = (test_data*)handle;
  test_async_data* ad = (test_async_data*)driver_alloc(sizeof(test_async_data));
  size_t p, q;
  char method, fn, arg, res;
  p = 0; q = 1;
  EV_GET_CHAR(ev, &method, &p, &q);
  EV_GET_CHAR(ev, &fn, &p, &q);
  EV_GET_CHAR(ev, &arg, &p, &q);
  switch (method) {
  case 1:
    res = (char) do_f(fn, arg);
    driver_output(d->port, &res, 1);
    break;
  case 2:
    ad->fn = fn;
    ad->arg = arg;
    driver_async(d->port, NULL, do_async, (void *) ad, NULL);
  }
}

static ErlDrvSSizeT test_drv_control(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen) {
  *rbuf[0] = (char) do_f((char) command, buf[0]);
  return 1;
}

static void ready_async(ErlDrvData handle, ErlDrvThreadData async_data) {
  test_data* d = (test_data*)handle;
  test_async_data* ad = (test_async_data *) async_data;
  driver_output(d->port, &(ad->res), 1);
}

static ErlDrvSSizeT test_drv_call(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen, unsigned int *flags) {
  int index = 0, vsn;
  char arg, res;
  ei_decode_version(buf, &index, &vsn);
  ei_decode_char(buf, &index, &arg);
  res = (char) do_f((char) command, arg);
  index = 0;
  ei_encode_version(*rbuf, &index);
  ei_encode_char(*rbuf, &index, res);
  return index+1;
}

ErlDrvEntry test_driver_entry = {
  test_drv_init,	        /* F_PTR init, called when driver is loaded */
  test_drv_start,		/* L_PTR start, called when port is opened */
  test_drv_stop,		/* F_PTR stop, called when port is closed */
  test_drv_output,		/* F_PTR output, called when erlang has sent */
  NULL,			        /* F_PTR ready_input, called when input descriptor ready */
  NULL,			        /* F_PTR ready_output, called when output descriptor ready */
#ifdef OUTPUTV
  "test_outputv_drv",           /* char *driver_name, the argument to open_port */
#else
  "test_drv",		        /* char *driver_name, the argument to open_port */
#endif
  test_drv_finish,	        /* F_PTR finish, called when unloaded */
  NULL,                         /* void *handle, Reserved by VM */
  test_drv_control,	        /* F_PTR control, port_command callback */
  NULL,			        /* F_PTR timeout, reserved */
#ifdef OUTPUTV
  test_drv_outputv,             /* F_PTR outputv, reserved */
#else
  NULL,                         /* F_PTR outputv, reserved */
#endif
  ready_async,                  /* F_PTR ready_async, only for async drivers */
  NULL,                         /* F_PTR flush, called when port is about
                                   to be closed, but there is data in driver
                                   queue */
  test_drv_call,                /* F_PTR call, much like control, sync call
                                   to driver */
  NULL,                         /* F_PTR event, called when an event selected
                                   by driver_event() occurs. */
  ERL_DRV_EXTENDED_MARKER,      /* int extended marker, Should always be
                                   set to indicate driver versioning */
  ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be
                                     set to this value */
  ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be
                                     set to this value */
  0,                            /* int driver_flags, see documentation */
  NULL,                         /* void *handle2, reserved for VM use */
  NULL,                         /* F_PTR process_exit, called when a
                                   monitored process dies */
  NULL                          /* F_PTR stop_select, called to close an
                                   event object */
};

#ifdef OUTPUTV
DRIVER_INIT(test_outputv_drv) /* must match name in driver_entry */
#else
DRIVER_INIT(test_drv) /* must match name in driver_entry */
#endif
{
  return &test_driver_entry;
}
