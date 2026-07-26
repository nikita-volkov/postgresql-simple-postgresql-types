# Upcoming

# v0.1.2.0

## Non-breaking

- Added `ToField`/`FromField` instances for `Citext` and `Geometry` (PostGIS).
- Upgraded to `postgresql-types` v0.1.5 and `postgresql-types-algebra` v0.2. `IsScalar` is renamed to `IsPrimitive` upstream; this package's internal `ViaIsScalar` helper is renamed to `ViaIsPrimitive` accordingly. Transparent to callers, since neither name was part of the public API.
