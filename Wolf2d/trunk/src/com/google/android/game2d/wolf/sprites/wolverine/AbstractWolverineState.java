package com.google.android.game2d.wolf.sprites.wolverine;

public abstract class AbstractWolverineState {
	
	public abstract void moveLeft(WolverineSprite sprite);
	public abstract void moveRight(WolverineSprite sprite);
	public abstract void moveDown(WolverineSprite sprite);
	public abstract void moverUp(WolverineSprite sprite);
	
	public static AbstractWolverineState getLeftState() {
		return new LeftState();
	}
	
	public static AbstractWolverineState getRightState() {
		return new RightState();
	}
	
}
