package com.google.android.game2d.wolf.sprites.wolverine;


public class LeftState extends AbstractWolverineState {

	public void moveDown(WolverineSprite sprite) {
		// TODO Auto-generated method stub
	}

	public void moveLeft(WolverineSprite sprite) {
		int column = sprite.getCurrentColumn() == 3 ? 8 : sprite.getCurrentColumn()-1;
		int row = 2;
		sprite.setFrame(column, row);
		sprite.setLeft(sprite.getLeft()-5);
	}

	public void moveRight(WolverineSprite sprite) {
		int column = 0;
		int row = 0;
		sprite.setFrame(column, row);
		sprite.setState(AbstractWolverineState.getRightState());
	}

	public void moverUp(WolverineSprite sprite) {
		// TODO Auto-generated method stub
		
	}
	

}
