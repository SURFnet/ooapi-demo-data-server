# Instant OOAPI

Instant OOAPI - just add water.

## More info soon...

## Chaos mode
You can set chaos mode as active by setting the environment variable `CHAOS` to `true`. When chaos mode is active, requests will be handled using a randomly picked `mode`. The following modes are available:
- `normal`: The request is handled as it normally would.
- `empty`: Returns an empty response.
- `slow`: Requests are handled normally, but with a randomly chosen delay between 3 and 63 seconds.

Which modes are active can be chosen by setting the environment variable `CHAOS_MODES` to a comma-delimited string of modes. This defaults to `normal,empty,slow`.