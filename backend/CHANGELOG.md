# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Initial project structure with Haskell backend
- Domain models for Options, Strategies, Positions, and Orders
- Servant API endpoints for Health, Auth, Strategies, Positions, Orders, and Settings
- Polysemy effects for Auth, Log, OKX, Position, Settings, and OrderBook
- Frontend foundation with Vite, React, TypeScript, and Tailwind CSS
- Zustand stores for global state management
- Shadcn/ui component library integration
- React Router for navigation
- Three main pages: Opportunities, Positions, Settings
- WebSocket client for real-time price updates
- OKX API integration structure
