<!-- Context: project-intelligence/technical | Priority: critical | Version: 1.0 | Updated: 2026-04-04 -->

# Technical Domain

**Purpose**: Tech stack, architecture, and coding patterns for Haskell backend + TypeScript/React frontend.
**Last Updated**: 2026-04-04

## Quick Reference
**Update Triggers**: Tech stack changes | New patterns | Architecture decisions
**Audience**: Developers, AI agents

## Primary Stack
| Layer | Technology | Version | Rationale |
|-------|-----------|---------|-----------|
| Backend Language | Haskell (GHC) | 9.x | Type safety, purity, modern extensions |
| Effect System | Polysemy | latest | Composable effects, testability |
| API Framework | Servant | latest | Type-safe APIs, auto docs |
| Frontend | React | 18+ | Concurrent features, hooks |
| Language | TypeScript | 5.x | Strict types, modern features |
| Build | Vite | latest | Fast HMR, modern esbuild |
| Testing | HSpec/Vitest | latest | Property + unit testing |
| Styling | Tailwind CSS | latest | Utility-first, rapid dev |

## Code Patterns

### Haskell API Endpoint (Servant + Polysemy)
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.User where

import Servant
import Domain.User (User, UserId)
import Effects.User (UserEffect(..))
import Polysemy (Members, Sem)

type UserAPI = 
  "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "id" UserId :> Get '[JSON] User

userServer :: Members '[UserEffect, Error AppError] r 
           => ServerT UserAPI (Sem r)
userServer = listUsers :<|> getUser
  where
    listUsers = listAllUsers @UserEffect
    getUser uid = fetchUser uid >>= maybe (throw NotFound) pure
```

### React TypeScript Component
```typescript
// components/UserCard/UserCard.tsx
import { useCallback, useState } from 'react';
import type { User } from '@/domain/user';
import { useUser } from '@/hooks/useUser';
import styles from './UserCard.module.css';

interface UserCardProps {
  userId: string;
  onUpdate?: (user: User) => void;
}

export function UserCard({ userId, onUpdate }: UserCardProps) {
  const { user, isLoading } = useUser(userId);
  const [isEditing, setIsEditing] = useState(false);

  const handleSave = useCallback((updated: User) => {
    onUpdate?.(updated);
    setIsEditing(false);
  }, [onUpdate]);

  if (isLoading) return <UserCardSkeleton />;
  return (
    <article className={styles.card}>
      <h3>{user.name}</h3>
    </article>
  );
}
```

## Naming Conventions
| Type | Convention | Example |
|------|-----------|---------|
| Haskell Modules | PascalCase | `Domain.User`, `API.User` |
| Haskell Functions | camelCase | `getUserById` |
| Haskell Types | PascalCase | `User`, `AppError` |
| TS Components | PascalCase | `UserCard.tsx` |
| TS Hooks | camelCase `use` | `useUser.ts` |
| Database | snake_case | `user_profiles` |

## Haskell Standards
- Enable `-Wall -Werror` for strict warnings
- Use `ExplicitForAll` and `ScopedTypeVariables`
- Prefer `newtype` over `type` for type safety
- Use `DerivingVia` for boilerplate reduction
- Leverage `Polysemy` for effects
- Write property-based tests with `QuickCheck`
- Use `fourmolu`/`ormolu` for formatting
- Prefer total functions

## TypeScript/React Standards
- Strict TypeScript mode (`strict: true`)
- Explicit return types on exports
- Functional components with hooks
- CSS Modules or Tailwind
- Co-locate tests (`*.test.tsx`)
- Use `React.memo` for expensive renders
- Zod for runtime validation

## Security Requirements
- **Input Validation**: API boundary validation (Servant)
- **Type Safety**: Make illegal states unrepresentable (Haskell)
- **SQL Injection**: Parameterized queries (persistent)
- **XSS Prevention**: React escaping, sanitize output
- **CSRF Protection**: Tokens for state changes
- **Authentication**: JWT or secure sessions
- **Authorization**: RBAC at API level
- **Secrets**: Environment variables only
- **HTTPS**: Enforce TLS
- **CORS**: Proper policies

## Codebase References
**Backend**: `backend/src/` - Servant API, Polysemy effects
**Frontend**: `frontend/src/` - React components, hooks
**Config**: `backend/*.cabal`, `frontend/package.json`

## Related Files
- Business Domain (business-domain.md)
- Decisions Log (decisions-log.md)
